# clails ジョブキューガイド

## 概要

このガイドでは、clailsのバックグラウンドジョブキューについて説明します。

[タスクシステム](task_ja.md)（`deftask`/`clails task`）がCLIから一度だけ同期的に
コマンドを実行するのに対し、ジョブキューはアプリケーションコードから「後で実行する
処理」をキューに積んでおき、リクエスト処理から切り離してワーカープロセスに実行させる
ための機能です。他のフレームワークでいうSidekiqやActiveJobに相当します。
ジョブはデータベースのテーブルに永続化されるため（外部ブローカーは不要）、ワーカーの
再起動をまたいでも消えず、ハンドラがエラーを送出した場合は指数バックオフで自動的に
リトライされます。

## 基本概念

- ジョブのハンドラは`defjob`マクロで定義（`deftask`の非同期版）
- `enqueue-job`でジョブを`clails_jobs`テーブルに永続化し、後で実行されるようにする
- ワーカー（`clails/job:run-worker-loop`、またはCLIの`clails job:work`）が
  このテーブルをポーリングし、実行期限が来たジョブを実行する
- 失敗したジョブは指数バックオフでリトライされ、成功するか`:max-attempts`に
  達すると`:failed`としてエラー内容とともに記録される
- `clails_jobs`テーブルは通常のマイグレーションで作成される。
  `db:migrate`/`db:rollback`の扱いに特別な点はない

## スコープ

このジョブキューは意図的に小さく、単一プロセス向けに設計されています。

- **対象範囲**: ジョブの永続化、実行期限が来たジョブをワーカーが取得して実行、
  `:max-attempts`まで指数バックオフでリトライ、指定時刻以降に実行（`:run-at`）
- **対象外**（現時点では）: 外部ブローカー（Redis/Sidekiq的なもの） --
  キューは完全にDB永続化のみで実現しています。ジョブを確認するためのWeb
  ダッシュボード。cron的な定期実行スケジュール（ジョブは`:run-at`以降に
  一度だけ実行されます）。[今後の方向性](#8-今後の方向性)を参照してください。

---

## 1. `clails_jobs`テーブルのセットアップ

ジョブキューにはジョブを永続化するテーブルが必要です。他のマイグレーションと
同じ要領で一度だけ生成し、実行してください。

```bash
clails generate:job-queue-setup
clails db:migrate
```

`generate:job-queue-setup`は、`clails_jobs`テーブルの定義
（`template/generate/job-queue-migration.lisp.tmpl`参照）が
あらかじめ書き込まれた、通常のマイグレーションファイルを`db/migrate/`に
生成するだけです。新しいマイグレーションの仕組みを覚える必要はありません。
以下のカラムが作成されます。

| カラム | 型 | 説明 |
|--------|------|------|
| `id` | integer | 主キー（clailsの全テーブルと同様に自動付与） |
| `job_name` | string | `defjob`で登録したジョブ名 |
| `arguments` | text | ジョブの引数（JSONエンコード） |
| `status` | string | `pending` / `running` / `succeeded` / `failed` |
| `attempts` | integer | これまでの試行回数 |
| `max_attempts` | integer | 諦めるまでに許容する試行回数 |
| `available_at` | datetime | このジョブを実行してよい最も早い時刻 |
| `last_error` | text | 直近の失敗時のエラーメッセージ |
| `created_at` / `updated_at` | datetime | clailsの全テーブルと同様に自動付与 |

プロジェクトでジョブキューを使わないのであれば、このテーブルを作成する
必要は一切ありません。他のclailsの機能はこのテーブルの存在を前提としません。

---

## 2. ジョブの定義

`defjob`マクロを使ってジョブのハンドラを登録します。

```common-lisp
(defpackage #:your-app/jobs/send-welcome-email
  (:use #:cl)
  (:import-from #:clails/job
                #:defjob)
  (:import-from #:your-app/models/user
                #:<user>))

(in-package #:your-app/jobs/send-welcome-email)

(defjob :send-welcome-email
  :description "新規ユーザーにウェルカムメールを送信"
  :max-attempts 5
  :function (lambda (&key user-id)
              (let ((user (find-user user-id)))
                (send-mail (ref user :email) "ようこそ！"))))
```

- `:function`には、`enqueue-job`に渡したのと同じキーワード引数を受け取る
  ラムダを指定します。
- `:max-attempts`（デフォルト25）は、このジョブを`:failed`として諦めるまでに
  ワーカーが試行する回数（1回目の実行を含む）です。`enqueue-job`呼び出し側で
  上書きすることもできます。
- タスクファイルと同様、ジョブファイルはアプリケーションの一部として
  読み込まれるようにしてください（例: `app/jobs/*.lisp`をアプリケーション
  ローダーに組み込む）。ジョブをenqueueしたりワーカーを実行したりする
  時点までに`defjob`が実行されている必要があります。

ジョブハンドラがエラーを送出することが、そのままキューに対する
「リトライしてほしい」という合図になります。よほどの理由がない限り、
ハンドラ内でエラーを自分で握りつぶさず、そのまま伝播（あるいは明示的に
`(error ...)`）させてください。

---

## 3. ジョブのenqueue

```common-lisp
(clails/job:enqueue-job :send-welcome-email :arguments (list :user-id 42))
```

`enqueue-job`は以下を受け取ります。

| 引数 | 説明 |
|------|------|
| `job-name` | `defjob`で登録したキーワード |
| `:arguments` | 実行時に`:function`へ渡すplist |
| `:max-attempts` | この呼び出しに限りジョブ自身の`:max-attempts`を上書き |
| `:run-at` | このジョブが実行可能になる時刻（universal time）。省略時は即時 |

```common-lisp
;; ワーカーが空き次第すぐ実行
(enqueue-job :send-welcome-email :arguments (list :user-id 42))

;; 登録時より多くリトライさせたい場合
(enqueue-job :send-welcome-email :arguments (list :user-id 42) :max-attempts 10)

;; 1時間後まで実行しない
(enqueue-job :send-welcome-email
             :arguments (list :user-id 42)
             :run-at (+ (get-universal-time) 3600))
```

`enqueue-job`は作成されたジョブの`id`を返します。

---

## 4. ワーカーの実行

### CLIから

```bash
clails job:work
```

コネクションプールを起動し、アプリケーションのモデルのメタ情報を読み込んだ上で
（`clails server`や`clails db:seed`と同じ起動処理）、`clails_jobs`テーブルを
永続的にポーリングし、実行期限が来たジョブを順次実行します。Ctrl-Cで停止します。

```bash
# ポーリング間隔を広げる
clails job:work --poll-interval 5
```

### アプリケーションコードから

```common-lisp
;; 現在のスレッドで無限に実行
(clails/job:run-worker-loop)

;; 別スレッドで実行
(clails/job:start-worker :poll-interval 1)
...
(clails/job:stop-worker)

;; 回数を区切って実行（スクリプトやテストで便利）
(clails/job:run-worker-loop :max-iterations 100 :poll-interval 1)
```

`start-worker`/`stop-worker`は、clailsの他の部分と同じスレッドモデル
（`clails/model/connection`参照）である`bordeaux-threads`のスレッド上で
ポーリングループを実行します。ワーカースレッドは、ジョブを取得または
完了させる最初のタイミングで、遅延的に自分専用のプールコネクションを取得します。

---

## 5. リトライとバックオフ

ジョブのハンドラがエラーを送出すると、ワーカーは以下を行います。

1. エラーメッセージをジョブの行に記録する（`last_error`）。
2. 試行回数がまだ`max_attempts`未満であれば、指数バックオフの遅延で
   再スケジュールする。`available_at`は`*default-backoff-base-seconds*`
   （デフォルト5秒）を試行のたびに倍にした値だけ先に延ばされ、
   `*default-backoff-max-seconds*`（デフォルト3600秒）で頭打ちになります
   （5秒、10秒、20秒、40秒、…と最大1時間まで）。ジョブの`status`は
   `pending`に戻るため、どのワーカーでも実行期限が来れば再度取得できます。
3. そうでなければ、ジョブを`:failed`として記録します。これ以降は
   リトライされません。

```common-lisp
;; バックオフの間隔をグローバルに調整する
;; （テストで使う場合や、5秒では速すぎ/遅すぎる場合など）
(setf clails/job:*default-backoff-base-seconds* 1)
(setf clails/job:*default-backoff-max-seconds* 60)
```

登録されていない名前のジョブ（例えば、`defjob`を呼ぶファイルをまだ
ワーカーが読み込んでいない場合）も、ハンドラのエラーと同様に扱われます。
起動順序がずれて先にワーカーが立ち上がった場合でも、後から追いつく
チャンスとしてリトライされ、`max_attempts`に達してもまだ未登録であれば
`:failed`になります。

---

## 6. 並行性とロック

ジョブの取得（claim）は1つのトランザクション内で行われます。
MySQL/PostgreSQLでは`SELECT ... FOR UPDATE SKIP LOCKED`、SQLite3では
`BEGIN IMMEDIATE`トランザクション（悲観的ロックのために
`clails/model/lock:with-locked-transaction`が使っているのと同じ
トランザクションモードの仕組みです。[document/model_ja.md](model_ja.md#8-悲観的ロック)
を参照）を使い、その後に行を`running`にして試行回数を増やす`UPDATE`を
実行します。そのため、同じテーブルに対して複数のワーカー（複数スレッド、
あるいは複数の`clails job:work`プロセス）を動かしても安全です。1つの行が
複数のワーカーに同時に取得されることはありません。

一方で、この行単位のclaim以上のプロセス間協調（リーダー選出、分散
スケジューリング、ノードをまたいだレート制御など）は提供していません。
1つのデータベースに対して1つのアプリケーションが動く構成であれば、
通常はこれで十分です。

---

## 7. 完全な使用例

```common-lisp
;; app/jobs/send-email.lisp
(defpackage #:your-app/jobs/send-email
  (:use #:cl)
  (:import-from #:clails/job
                #:defjob)
  (:import-from #:clails/logger/core
                #:log.job))

(in-package #:your-app/jobs/send-email)

(defjob :send-email
  :description "トランザクションメールを送信"
  :max-attempts 5
  :function (lambda (&key to subject body)
              (log.job "メール送信中" :to to :subject subject)
              (your-app/mailer:deliver :to to :subject subject :body body)))
```

```common-lisp
;; 注文作成後、コントローラのアクションなどから
(clails/job:enqueue-job :send-email
                        :arguments (list :to (ref order :customer-email)
                                        :subject "ご注文確認"
                                        :body (render-order-confirmation order)))
```

```bash
# 初回のみ
clails generate:job-queue-setup
clails db:migrate

# 別プロセス/別ターミナルでワーカーを起動し続ける
clails job:work
```

`your-app/mailer:deliver`がエラーを送出した場合（メールサーバーに
一時的に接続できない、など）、ジョブは自動的にリトライされます。5秒後、
10秒後、20秒後、40秒後と合計5回試行した後、`:failed`となり、元の
エラーが`last_error`に記録されるので、後から確認できます
（例: `select * from clails_jobs where status = 'failed'`）。

---

## 8. 今後の方向性

今回は意図的に対象外とした、より大きな設計判断が必要な項目です。

- **外部ブローカー連携**（Redis/Sidekiq、RabbitMQ、SQSなど）。
  プロセスや言語をまたいだキューや、ポーリング型のDBテーブルより高い
  スループットが必要なアプリケーション向け。これは
  `enqueue-job`/`defjob`という同じAPIの背後にある別バックエンドとして
  追加されるべきものであり、現在の仕組みの置き換えではありません。
  新たな必須依存が増えるため、別途の提案に委ねます。
- 保留中・失敗したジョブを確認するための**Webダッシュボード**
  （Sidekiq Webや`good_job`のダッシュボードに相当するもの）。
- 既存の一度きりの`:run-at`スケジューリングに加えた、**cron的な
  定期実行スケジュール**（「毎日午前2時に」など）。
- **キュー/優先度のサポート**（複数の名前付きキュー、キューごとの
  ワーカー同時実行数の制御）。

---

## 9. トラブルシューティング

### ジョブが実行されない

- ワーカーは実際に動いていますか（`clails job:work`、あるいは
  アプリケーション内の`start-worker`/`run-worker-loop`）？
- `clails_jobs`テーブルは存在しますか（`clails generate:job-queue-setup`
  と`clails db:migrate`）？
- 行の`available_at`を確認してください。未来の`:run-at`でenqueueされた
  ジョブは、その時刻になるまで取得されません。

### ジョブがすぐに「No job registered for name ...」で失敗する

ワーカープロセスが、そのジョブ名を`defjob`しているファイルを読み込んで
いません。モデルやコントローラと同様に、ジョブファイルがアプリケーション
起動時に読み込まれるようにしてください。

### ジョブがリトライを繰り返して成功しない

行の`last_error`で元の例外メッセージを確認してください。
`max_attempts`に達すると、ジョブは`:failed`になります。

---

## 10. まとめ

- `defjob`でジョブのハンドラを定義し、`enqueue-job`で処理をキューに積む
- `clails generate:job-queue-setup`を一度実行し、続けて`clails db:migrate`
  を実行して`clails_jobs`テーブルを作成する
- `clails job:work`（またはコードから`run-worker-loop`/`start-worker`）で
  ワーカーを実行する
- 失敗したジョブは`max-attempts`まで指数バックオフで自動的にリトライされ、
  それでも失敗した場合はエラーとともに`:failed`になる

メール送信や外部APIの呼び出し、アップロードされたファイルの処理など、
遅かったり失敗しうる処理をリクエスト処理の中で直接行いたくなったときは、
ジョブキューの利用を検討してください。
