# clails セッション・認証ガイド

## 概要

clails には、[`lack-middleware-session`](https://github.com/fukamachi/lack)
をベースにした Cookie セッションの段と、そのセッションの上に構築された
小さな `current-user` 拡張ポイントが用意されています。

まず押さえておきたい既定値が2つあります。

- **`clails new` で生成したプロジェクトでは、セッションは有効になっています。**
  有効化している行は `app/config/environment.lisp` にコメント付きで書かれて
  おり、最初のリクエストから `(session controller)` が使えます。
- **フレームワーク本体の既定は「無効」です。** セッションについて何も書いて
  いない既存プロジェクトの挙動は一切変わりません(Cookie は発行されず、
  `(session controller)` は「段を有効にしてください」というエラーになります)。
  今回の変更が既存プロジェクトに自動的に及ぶことはありません。
  [9. 既存プロジェクトの移行手順](#9-既存プロジェクトの移行手順) を参照してください。

clails が提供するもの／しないもの:

- **セッション機能** はフレームワークが提供する実質的な機能です。リクエスト
  ごとの Cookie ベースのハッシュテーブルに加えて、サーバ側の有効期限、
  掃除、セッション単位のロック、ストアの差し替えを提供します。
- **`current-user`** は「配線」のみです。clails はパスワードのハッシュ化、
  ログイン/ログアウト用ルーティング、「ユーザー」モデルの規約など、認証の
  仕組みそのものは一切提供しません。「現在ログインしているユーザーが誰か」
  を保存するための、ドキュメント化された置き場所を提供するだけです。
  ログインロジックはプロジェクト側で実装し、成功したら
  `(setf (current-user controller) ...)` を呼び出します。

明示的に **対象外**(clails が提供しないもの):

- Remember-me(永続ログイン)トークン
- OAuth / OIDC 連携
- ロールベースの認可・パーミッションチェック
- 組み込みのパスワードハッシュ化ヘルパー

### CSRF 対策についての注記

このリリースでは CSRF 対策を有効にしません。したがって生成されたプロジェクト
に CSRF 対策はありません。これは後退ではありません。このリリース以前に
セッションを有効にしていたプロジェクトにも CSRF 対策はなく、今回加わったのは
セッション側の健全性(並行要求でセッションが壊れないこと、有効期限と掃除、
Cookie の既定を安全側に寄せたこと)です。CSRF 対策は次の作業で扱う予定で、
生成テンプレートでも有効化しておらず、本ガイドでもまだ説明しません。

---

## 1. 設定

セッションの設定項目はいずれもセッション専用のパッケージで宣言され、
`clails/middleware/session-middleware` から再公開されています(さらに
`clails/middleware` からも再公開されています)。したがって1つのパッケージで
すべてに到達できます。

**これらはすべて `clails server` がミドルウェアスタックを組み立てるときに
1度だけ読まれます。** 設定は `app/config/environment.lisp`(サーバ起動前に
読み込まれるファイル)に書いてください。稼働中のイメージで値を変えても、
動いているサーバの応答は変わりません。

### 生成されたプロジェクトに入っている内容

```common-lisp
(in-package #:myapp/config/environment)

;; clails の Cookie セッションの段を有効にする
(setf *enable-session-middleware* t)

;; NIL = clails のインメモリストア(DB ストアは第6節)
(setf *session-store* nil)

;; 放置されたセッションが失効するまでの秒数
(setf *session-idle-timeout* 1800)

;; 失効したセッションは定期掃除でも削除される
(setf *session-sweep-enabled* t)
(setf *session-sweep-interval* 300)
```

Cookie の安全通信限定(Secure)だけは環境ごとに値が異なるため、生成される
設定ファイルでは環境ごとの関数(`setup-session/develop`、
`setup-session/test`、`setup-session/production`)に分かれており、
`clails.boot` が `CLAILS_ENV` を解決したあとに `initialize-session-config`
を呼び出します。[2. セッション Cookie](#2-セッション-cookie) を参照して
ください。

### 設定項目一覧

| 設定項目 | 意味 | フレームワーク既定値 | 推奨値 |
|---|---|---|---|
| `*enable-session-middleware*` | セッションの段が機能するかどうか | `NIL`(無効) | `T` — 生成プロジェクトは設定済み |
| `*session-store*` | セッションの保存先。`NIL` はインメモリストア | `NIL` | 開発はインメモリ、本番は DB ストア |
| `*session-state*` | Cookie。`NIL` は clails 自身の状態(下の Secure を反映) | `NIL` | Cookie 名・パス・ドメイン・SameSite を変えたいとき以外はそのまま |
| `*session-cookie-secure*` | Cookie に Secure を付ける(HTTPS 以外では送られない) | `NIL` | HTTPS で公開する環境では `T`、平文の開発・試験では `NIL` |
| `*session-idle-timeout*` | 放置してよい秒数。`NIL` でサーバ側の失効を無効化 | `1800`(30 分) | 1800。機密性の高い用途ではより短く |
| `*session-sweep-enabled*` | 定期掃除スレッドを動かすか | `T` | `T` |
| `*session-sweep-interval*` | 掃除の間隔(秒) | `300`(5 分) | 300。間隔を延ばしても失効済みセッションが有効になることはない |

セッションの段は常にミドルウェアスタックの決まった位置にあります(トランザ
クションの段と振り分けの段より外側でなければならないため)。したがって
セッションを有効にするために `*clails-middleware-stack*` を編集する必要は
ありません。`*enable-session-middleware*` が `NIL` のときは単なるパススルー
です。スタック全体については [環境設定ガイド](environment_ja.md) を参照して
ください。

---

## 2. セッション Cookie

clails が既定で発行する Cookie は次の形です。

```
_session_id=<16 進 40 文字>; path=/; HttpOnly; SameSite=Lax
```

- **HttpOnly** — スクリプトからセッション ID を読めません。
- **SameSite=Lax を明示** しています。属性を省略することは「未指定」には
  なりません。下位の応答組み立てが `None` を既定にし、さらに毎回警告を出す
  ためです。
- **実装を推測されない Cookie 名。** 以前の既定値は下位ライブラリ由来の
  `lack.session` で、応答を見た相手にどのライブラリ(つまりどの既知の弱点)を
  試せばよいかを教えてしまっていました。
- **失効日時は意図的に持ちません。** 期限はサーバ側の放置時間(第5節)です。
  Cookie 側で同じ方針は表現できません。段が `Set-Cookie` を付けるのは
  セッションの作成時・ID 変更時・破棄時だけなので、Cookie に置いた日時は
  更新されず、「作成時刻からの絶対期限」になってしまいます。ブラウザから
  見ればこの Cookie はセッション Cookie(ブラウザを閉じると消える)であり、
  サーバ側の放置時間だけが期限として働きます。

### Secure: 環境ごとに値が変わる唯一の設定

`T` にすると、ブラウザはこの Cookie を **HTTPS でのみ** 送信します。
フレームワークの既定は `NIL` で、これは「既定を安全側に寄せる」ことからの
意図的な例外です。Secure 付きの Cookie は平文 HTTP では **まったく** 送られ
ないため、TLS なしで動かす開発環境でこれを有効にするとセッションが成立しなく
なります(Cookie は発行されるが戻ってこないので、毎回が初回訪問に見えます)。

**clails は実行中の環境を判定しません。** `*session-cookie-secure*` だけを
読みます。だからこそ環境差はプロジェクトの設定ファイルが担います。

```common-lisp
(defun setup-session/develop ()
  (setf *session-cookie-secure* nil))   ; 平文 HTTP

(defun setup-session/test ()
  (setf *session-cookie-secure* nil))   ; 平文 HTTP

(defun setup-session/production ()
  (setf *session-cookie-secure* t))     ; HTTPS のみ
```

### Cookie を変更する

```common-lisp
(setf clails/middleware/session-middleware:*session-state*
      (clails/middleware/session/state/cookie:make-clails-cookie-state
        :cookie-key "_myapp_session"
        :path "/"
        :samesite :strict))
```

自分で組み立てた状態は `:secure` を自分で持つため、
`*session-cookie-secure*` は参照されなくなります。ここには lack 自身の
Cookie 状態も渡せます。ブラウザを閉じても残る **永続 Cookie** が必要な場合は
そちらを使ってください(clails の状態は意図的に表現できません)。いずれの
場合もサーバ側の放置時間は適用されます。

---

## 3. セッションデータの読み書き

コントローラのインスタンスには `session` アクセサがあり、Cookie セッション
に紐づくリクエストごとのハッシュテーブル(`:test 'equal`)を返します。

```common-lisp
(defmethod do-get ((controller <my-controller>))
  (let ((visits (or (gethash "visits" (session controller)) 0)))
    (setf (gethash "visits" (session controller)) (1+ visits))
    (set-view controller "home/index.html"
              `(:visits ,(gethash "visits" (session controller))))))
```

ハッシュテーブルに入れた値は応答の生成後にストアへ書き戻され、クライアント
は Cookie を保持し続けるので、次のリクエストでも同じ内容を読めます。

`*enable-session-middleware*` が `NIL` のとき `(session controller)` は
エラーになり、段を有効にするよう案内します。黙って使い捨てのハッシュテーブル
を返すと、気づかないままデータを失いやすいからです。

### 予約キー: `clails.` 接頭辞

**`clails.` で始まるキーは枠組みのものです。** アプリケーションから書き込ま
ないでください。接頭辞は `+session-reserved-key-prefix+` として公開されて
おり、`(session-reserved-key-p key)` で判定できます。現在使われているのは
`"clails.current-user"` です。

それ以外のキーはすべてアプリケーションのものです。枠組みの項目を残したまま
セッションを空にしたいときは
`(session-clear controller :keep-reserved t)` を使ってください。

なお、失効判定の基準となる **最終アクセス時刻は予約キーではありません**。
セッションの値の中ではなくストア側に置かれているため(第5節)、ハッシュ
テーブルを走査しても現れません。

---

## 4. セッションのライフサイクル API

読み書きは `gethash` で行いますが、それ以外の操作は関数として提供されます。
いずれもコントローラを渡します(要求環境のプロパティリストも渡せます)。
`clails/controller/base-controller` から再公開されているので、生成された
コントローラではパッケージ修飾なしで呼べます。

| 呼び出し | 内容 |
|---|---|
| `(session-id controller)` | 現在のセッション ID |
| `(session-new-p controller)` | このリクエストがセッションを作ったか |
| `(session-value controller key &optional default)` | 1つのキーを既定値付きで読む |
| `(session-delete controller key)` | 1つのキーを忘れる |
| `(session-clear controller &key keep-reserved)` | セッション自体は残して全キーを忘れる |
| `(session-destroy controller)` | セッションを終わらせる(ストアから削除し Cookie を失効させる) |
| `(session-regenerate-id controller)` | 内容を保ったまま ID を振り直す |
| `(session-destroyed-p controller)` | このリクエストで `session-destroy` が呼ばれたか |
| `(session-id-changing-p controller)` | この応答で ID が変わるか |

このうち2つには注意書きがあります。

- **`session-regenerate-id` はセッション固定化攻撃への対策です。** 利用者の
  権限が変わった直後(ログイン成功後、ロールの昇格後)に呼んでください。
  事前にブラウザへ植え付けられた ID が、権限を持つセッションの ID に
  ならないようにするためです。
- **`session-destroy` は明示的なログアウト用です。** 内容は即座に消え、
  応答には Cookie を失効させる `Set-Cookie` が付きます。呼び出し後に
  セッションへ書いた内容は失われるので、応答を先に組み立て、破棄は最後に
  行ってください。

---

## 5. 有効期限と掃除

### 寿命ではなく「放置時間」

セッションは `*session-idle-timeout*` 秒(既定 30 分)**使われなかった**
ときに失効します。生きたセッションを伴うリクエストは毎回その期限を先へ
押し出すので、作業を続けている利用者が勝手にログアウトすることはありません。
`NIL` にするとサーバ側の失効は完全に無効になり、セッションは破棄されるか
ストアが失うまで残ります。

最終アクセス時刻は **ストアが** 持ちます。セッションの値の中には入れません。
これが方針を成り立たせている点です。もしハッシュテーブルのキーにすると、
1回のページ読み込みが並行して投げる静的ファイルの取得まで含めて、
**すべてのリクエストがセッションの書き込み側に回ってしまいます**。
ストア側に置けば、時刻だけを更新する操作(インメモリなら索引の1エントリ、
DB なら1列)で済みます。

これは `updated_at` の流用でもありません。DB ストアは専用の列を持ち、
保存時と時刻更新時の両方で書きます。他の仕組みが管理する列は「読むだけの
リクエスト」では書かれないため、放置時間方式のもとでは、まさに今ページを
見ている利用者を失効させてしまいます。

### 二重の掃除

- **アクセス時の遅延削除。** 失効判定は毎リクエスト、保存された時刻から
  行われます。したがって掃除の進み具合に関係なく、失効したセッションが
  有効として扱われることは **ありません**。
- **定期掃除スレッド。** 起動時フックで開始し、停止時フックで止めます。
  `*session-sweep-interval*` 秒ごとに生きている ID を走査し、期限を過ぎた
  ものを削除します。これがないと「誰も戻ってこないセッション」が永久に
  解放されません。

サーバの停止は現在の間隔の経過を待ちません。間隔を長くしても停止が遅く
なることはありません。

### 強制終了ではフックが走らない

`SIGKILL`、電源断、クラッシュではフックが走らずにプロセスが消えます。
これは「対処」ではなく「織り込み済み」です。

- インメモリストアならセッションはプロセスと一緒に消えるので、掃除対象は
  残りません。
- 永続ストアでは行が残りますが、次のプロセスの定期掃除か、その ID が提示
  されたときの遅延削除で消えます。永久に溜まることはなく、失効済みの
  セッションが有効になることもありません。

### 定期掃除をジョブキューにしなかった理由

clails にはジョブキューがあり、「5 分ごとに何かを実行する」はジョブのように
見えます。しかし次の3点のいずれもが単独で致命的です。

1. **ワーカは別プロセス。** ジョブは Web サーバの外で動く `clails job:work`
   が取り出しますが、既定のストアはサーバプロセスのメモリにセッションを
   置いています。
2. **繰り返しの予定表がない。** キューは指定時刻以降に1回実行するだけで、
   「N 秒ごと」の概念がありません。ジョブ自身が再登録する形になり、1度
   登録に失敗すると掃除が永久に止まります。
3. **DB のテーブルが必要。** セッションの失効は、DB を持たない既定構成でも
   動かなければなりません。

---

## 6. ストア

`*session-store*` には、clails の
`clails/middleware/session/store:<session-store>` を継承したクラスの
インスタンスか、lack のセッションストア構造体のどちらも渡せます。段から見た
扱いは同じですが、ストアプロトコルの第4の操作(`touch-session`、最終
アクセス時刻の記録)を実装できるのは clails 側の派生だけです。lack の構造体は
CLOS クラスを継承できないため最終アクセス時刻を持てず、放置時間による失効も
働きません。

### インメモリ(既定)

`NIL` のときはスタック組み立て時にインメモリストアが作られます。開発や単一
プロセスには適していますが、プロセスを止めるとセッションは失われ、2つ目の
プロセスとは共有されません。

### データベース(本番の推奨構成)

再起動後もセッションが残り、同じテーブルを読むすべてのプロセスで共有され
ます。

```common-lisp
(setf clails/middleware/session-middleware:*session-store*
      (clails/middleware/session/store/db:make-db-session-store))
```

このストアは操作ごとに専用の接続を借りて返します。セッションの段は
トランザクションの段より **外側** にあるため、リクエストの接続で書くと
セッションの書き込みがアプリケーションのトランザクションに巻き込まれ、
ロールバックで黙って巻き戻ってしまいます。

**DB に保存するセッションには、印字して読み戻せる値しか入れられません**
(文字列、数、シンボル、リスト、ベクタ、それらのハッシュテーブル)。
クラスのインスタンスやストリームは保存できず、読み戻せない文字列を書く
代わりにエラーになります(インメモリストアにはこの制約はありません。
第8節の `current-user` も参照)。

#### テーブルを作る

```bash
clails db:create-session-table
```

- **定義体はプロジェクトの `db/session-table.lisp`** で、`clails new` が
  写します。内容を確認・編集できます。ストアも同じファイルから列名を読む
  ので、列名を変えれば両側が一緒に動きます。このファイルができる前に作られた
  プロジェクトでは、同じ内容のフレームワーク側の定義が使われます。
- **これはマイグレーションではありません。** ファイルはデータであり、読まれる
  だけで評価されません。テーブルはマイグレーション履歴にも `db/schema.lisp`
  にも現れず、`clails db:migrate` や `clails db:status` は作成も変更も
  しません。セッションテーブルはアプリケーションのスキーマではなく枠組みの
  管理情報だからです。
- **1段階で、2回実行しても安全です。** コマンドは
  `CREATE TABLE IF NOT EXISTS` を1文組み立てて実行します。2回目は何もせず、
  既にあるセッションもそのまま残ります。あとから適用する中間ファイルは
  ありません。
- **方言差は文の組み立て時に解決されます。** 同じ定義ファイルで SQLite3・
  MySQL・PostgreSQL に対応します(`datetime` と `timestamp` の綴りの違いなど)。
- **テーブルを削除するコマンドは意図的にありません。** テーブルにアプリ
  ケーションのデータは入っておらず、打ち間違いで全利用者がログアウトする
  ようなコマンドは置きません。削除が必要なら DB 側で `DROP TABLE` して
  ください。

生成されたプロジェクトでは `make db.create-session-table` が用意され、
README の「Setup Database」にも手順が載っています。

### 別のストアを作る

`<session-store>` を継承し、`fetch-session`・`store-session`・
`remove-session`(媒体が「取得→変更→保存」より良い手段を持つなら
`touch-session` も)を実装して `*session-store*` に入れてください。基底
クラスに `touch-session` の既定実装があるので、実装しなくても放置時間による
失効は働きます。

---

## 7. 同一セッションへの並行要求

### 枠組みが保証すること

1回のページ読み込みは同じセッション Cookie を持つ複数のリクエストに分かれる
ため、同一セッションへの並行アクセスは例外ではなく通常の状態です。そこで
セッションの段は、設定されたストアを包んで同一セッションへのアクセスを
直列化します。これにより **セッションの入れ物が壊れることはなくなります**
(テーブルの消失も、書きかけの値も起きません)。

### 保証しないこと

1回の読みと1回の書きをそれぞれ直列化しても、**その並び全体は原子的になり
ません**。2つのリクエストが同じ値を読み、変えて書き戻すと、片方の変更だけが
残ることがあります。これは欠陥ではなく仕様です。Rails や Django も同じ立場
です。

もう一方の案、すなわち **リクエスト全体でロックを保持する方式は検討のうえ
採っていません**。同じ利用者の複数のリクエストが互いの終了を待って並ぶことに
なり、しかも守る対象であるインメモリストアはそもそも本番構成ではありません。
そこを超える規模になったら共有ストアと複数プロセスへ移ることになり、1つの
イメージ内のリクエスト単位のロックはそこでは役に立ちません。

必要な場所では `with-session-lock` で並び全体を原子的にしてください。

```common-lisp
(with-session-lock (controller)
  (let ((count (or (gethash "count" (session controller)) 0)))
    (setf (gethash "count" (session controller)) (1+ count))))
```

「セッションを読んでから、読んだ値に基づいて書く」操作(カウンタの増加、
配列への追記、条件付き更新)では必ず使ってください。注意点は3つです。

- **本体は短く保つこと。** **同じ** セッションの他のリクエストは本体の終了を
  待ちます。問い合わせや外部 HTTP 呼び出しはロックの外に出してください。
  他のセッションのリクエストには一切影響しません。
- **入れ子は安全です。** ロックは再入可能です。
- **このプロセス内のロックです。** 下記を参照してください。

### 複数プロセス構成: 後勝ち

DB(将来の Redis)ストアを複数プロセスで共有する場合、**別のプロセス** が
処理する同一セッションへの並行要求では、書き戻しの片方が失われることが
あります。セッションの値は丸ごと書かれるため、後の書き込みが前の書き込みを
置き換えます。これは欠陥ではなく文書化された挙動で(ここも Rails や Django と
同じ立場です)、clails は楽観ロックや CAS を意図的に入れません。

- 既定構成である単一プロセスでは顕在化しません。
- **`with-session-lock` はここでは効きません。** 1つのイメージ内のロック
  であり、他プロセスのことは何も知りません。

---

## 8. `current-user`: 認証の仕組みではなく拡張ポイント

`current-user` は `session` の上に作られた薄いアクセサの対です。

```common-lisp
(current-user controller)              ; => 最後に保存した値、または NIL
(setf (current-user controller) user)  ; ログイン中の利用者として保存
(setf (current-user controller) nil)   ; 「ログアウト」
```

内部では `session` が返すのと同じハッシュテーブルの予約キー
(`"clails.current-user"`)を読み書きするだけです。別の保存領域もユーザー
モデルの要件もありません。`user` にはアプリケーションに都合のよいものを
置けます: `:id`/`:name` のプロパティリスト、毎リクエスト引き直す整数の ID、
モデルのインスタンスなど。

**DB セッションストアを使う場合の注意:** ここに置いた値はセッションの値の
一部になるため、印字して読み戻せる必要があります(第6節)。プロパティリスト
や ID は条件を満たしますが、モデルのインスタンスは満たしません。ID を保存して
毎リクエスト引き直すのが移植性のある選択です。

### ログイン/ログアウトの例(説明用)

**この例は意図的に最小限で、本番のセキュリティ水準ではありません。** API の
形を示すためのもので、パスワードのハッシュ化、時間差の出ない比較、流量制限は
いずれも行っていません。これらは自分で実装するか、専用のライブラリを使って
ください。

```common-lisp
(defclass <session-controller> (<rest-controller>)
  ())

;; POST /login  { "email": "...", "password": "..." }
(defmethod do-post ((controller <session-controller>))
  (let* ((email (param controller "email"))
         (password (param controller "password"))
         ;; find-user-by-email / verify-password はプロジェクト側のコード。
         ;; clails はユーザー検索もパスワード検証も提供しない。
         (user (find-user-by-email email)))
    (if (and user (verify-password user password))
        (progn
          ;; 権限が変わったので ID を振り直す(第4節)
          (session-regenerate-id controller)
          (setf (current-user controller) (list :id (ref user :id)
                                                :email (ref user :email)))
          (set-response controller '((:status . "ok"))))
        (progn
          (setf (slot-value controller 'code) 401)
          (set-response controller '((:status . "error")
                                     (:message . "invalid credentials")))))))

;; DELETE /login (ログアウト)
(defmethod do-delete ((controller <session-controller>))
  (set-response controller '((:status . "ok")))
  (session-destroy controller))
```

ログイン必須のコントローラは次のように書けます。

```common-lisp
(defclass <account-controller> (<web-controller>)
  ())

(defmethod do-get ((controller <account-controller>))
  (let ((user (current-user controller)))
    (if user
        (set-view controller "account/show.html" `(:user ,user))
        (set-redirect controller "/login"))))
```

この判定は素の Lisp なので、ヘルパー関数や `:before` メソッドに切り出す
などプロジェクトの作法に合わせて自由に整理できます。clails は特定の認可
パターンを強制しません。

---

## 9. 既存プロジェクトの移行手順

`clails new` はプロジェクトの設定を **1度だけ** 書き出します。今回の変更が
既存のプロジェクトに及ぶことはないので、以下はすべて手作業で、すべて任意
です(何も変えなければセッション無効のまま従来どおり動きます)。

1. **セッションの設定を追記する。** `app/config/environment.lisp` に追記
   します。使い捨てのプロジェクトを生成し(`clails new scratch`)、その
   `app/config/environment.lisp` から「Sessions」の塊を
   `setup-session/<環境>` 関数ごとコピーするのが簡単です。
2. **環境ごとの部分を boot から呼ぶ。** Secure はミドルウェアスタックの
   組み立て時に読まれ、これは最初の起動時フックより **前** に起こるため、
   起動時フックでは間に合いません。生成される `clails.boot` では
   `resolve-project-environment` の直後に
   `(<プロジェクト>/config/environment:initialize-session-config)` を
   呼んでいます。同じ行を自分の `clails.boot` に追加し、設定パッケージから
   この関数を公開してください。この手順を省くと、本番でも Cookie に Secure
   が付きません。
3. **`*clails-middleware-stack*` を丸ごと置き換えている場合**、その
   スタックにはセッションの段がないため、上記の設定を足しても有効になり
   ません。自分で足す段は
   `add-middleware-before` / `add-middleware-after` で足すように変更し、
   枠組みの段を本来の位置に残してください
   ([環境設定ガイド](environment_ja.md))。
4. **DB にセッションを置く場合**、生成したプロジェクトから
   `db/session-table.lisp` をコピーし(同じ内容の組み込み定義に任せることも
   できます)、環境ごとに `clails db:create-session-table` を実行して
   `*session-store*` に `(make-db-session-store)` を設定します。make の
   近道が欲しければ `db.create-session-table` ターゲットを自分の `Makefile`
   に足してください。生成される `Makefile` と `README.md` も一度しか写され
   ないため、既存プロジェクトには及びません。
5. **Cookie の既定が変わっていることに注意してください。** 以前のセッション
   は引き継がれません。Cookie 名が下位ライブラリの `lack.session` ではなく
   なったため、古い Cookie を持つブラウザは1度だけ新しいセッションを始めます
   (つまりログイン中の利用者は1度ログアウトされます)。新しい Cookie は
   HttpOnly で `SameSite=Lax` を明示し(サイト間の POST には送られなくなり
   ます)、失効日時を持ちません。従来の挙動に揃えたい場合は、自分で状態を
   組み立てて `:cookie-key "lack.session"` を渡してください(第2節)。
6. **本番の記録設定の誤りを直しました。** 生成される
   `app/config/logger.lisp` で、往復記録用の口が `:sql` という名前で登録され
   ていたため、`:web-access` が未登録になり、SQL の記録が
   `logs/access.log` に行って `logs/sql.log` が空のままになっていました。
   修正はテンプレート側なので既存プロジェクトには及びません。自分の
   `setup-logger/production` を新しく生成したものと見比べ、`:sql`・
   `:web-access`・`:audit`・`:task` がそれぞれ1回だけ登録されていることを
   確認してください。これは記録だけの問題ではありません。拒否した要求を
   記録するのは `:audit` の口です。

---

## まとめ

1. 生成されたプロジェクトではセッションは **有効**、フレームワークの既定は
   **無効** です。したがって既存プロジェクトは設定を足すまで影響を受けません。
2. `(session controller)` はリクエストスコープの Cookie 連動ハッシュテーブル
   です。`clails.` で始まるキーは枠組みの予約です。
3. セッションは既定で **30 分の放置** で失効し、遅延削除と定期掃除の両方で
   削除されます。Cookie は設計上、失効日時を持ちません。
4. Cookie は HttpOnly・`SameSite=Lax` で、実装を推測されない名前を使います。
   **Secure は既定で無効で、プロジェクトの設定が環境ごとに有効化します** —
   本番では有効、平文の開発では無効です。
5. 既定はインメモリストア、本番の推奨は DB ストアで、そのテーブルは
   `clails db:create-session-table` で1度だけ作ります。
6. 並行要求でセッションが壊れることはありませんが、「読んで変えて書く」では
   更新が失われることがあります。`with-session-lock` を使ってください。
   複数プロセスでは後勝ちになります。
7. `current-user` は「ログイン中の利用者」の配線です。ログイン処理・
   パスワードの扱い・認可はプロジェクトの責任で、CSRF 対策はこのリリースには
   含まれません。
