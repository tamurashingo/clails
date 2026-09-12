# clails 環境変数ガイド

## 概要

clails アプリケーションは、環境変数を通じて動作をカスタマイズできます。
このガイドでは、アプリケーション開発者が使用できる環境変数とグローバル変数、そして Middleware の設定について説明します。

## 目次

- [初期化ステージ](#初期化ステージ)
  - 3つのステージ
  - CLIコマンドごとに到達するステージ
  - 実務上の意味
1. [環境変数](#1-環境変数)
   - データベース関連
   - 環境変数の取得ユーティリティ
2. [グローバル変数](#2-グローバル変数)
   - プロジェクト関連
   - データベース関連
   - ルーティング関連
   - アプリケーションライフサイクル関連
3. [Middleware の設定](#3-middleware-の設定)
   - Middleware スタック
   - Middleware の追加
   - 組み込み Middleware
   - Middleware の実行順序
   - Middleware の使用例
4. [環境変数の設定方法](#4-環境変数の設定方法)
5. [設定ファイルの例](#5-設定ファイルの例)
6. [ベストプラクティス](#6-ベストプラクティス)
7. [トラブルシューティング](#7-トラブルシューティング)

---

## 初期化ステージ

アプリケーションコードが `clails/environment` の値に依存する前に、その値が実際に「いつ」設定されるのかを把握しておく必要があります。clails は3つのステージで状態を初期化しますが、どのステージまで到達するかは実行する `clails` CLI コマンドによって異なります。これが原因で、より後のステージで設定される変数（特に `*connection-pool*`）が既に利用可能だと仮定したコードが、`clails server` では問題なく動いても `clails db:seed` やその他のコマンドでは予期せず失敗することがあります。

### 3つのステージ

**ステージ1 -- ファイルロード時**

`clails` システム自体がロードされた時点で到達します。`--help` を含め、`clails` CLI のすべての呼び出しはこの時点に到達します。この段階では `src/environment.lisp` に定義されたフレームワークの組み込みデフォルト値のみが有効です。例えば `*project-environment*` は `:develop`、`*connection-pool*` は `nil`、`*routing-tables*` は組み込みのデフォルトルート1件のみ、`*startup-hooks*`/`*shutdown-hooks*` もフレームワーク自身のデフォルトのみです。この時点ではまだプロジェクトはロードされていません。

**ステージ2 -- `clails.boot` 実行（プロジェクトロード）時**

`new` コマンド（およびコマンドなし/`--help` での実行）以外のすべてのコマンドで到達します。`roswell/clails.ros` の `load-project` がプロジェクトの `clails.boot` をロードすることで発生します。順を追うと以下のようになります。

1. `(ql:quickload :<project>)` を実行します。これは `app/application-loader.lisp` を起点とする ASDF package-inferred-system の依存グラフをたどり、各ファイルのトップレベルフォームを実行します。プロジェクトの設定ファイルが実行されるのはこの時点です。`app/config/environment.lisp` が `*project-name*` と `*routing-tables*` を設定し、`*startup-hooks*`/`*shutdown-hooks*` に要素を push します。`app/config/database.lisp` が `*database-type*` を設定します。`app/models/package.lisp` がモデルを登録します。
2. `*project-dir*`、`*migration-base-dir*`、`*task-base-dir*` を明示的に設定します。
3. `CLAILS_ENV`（`set-environment` 経由）を適用し、`*project-environment*` を設定します。
4. `<project>/config/database:initialize-database-config` を呼び出し、`*database-config*` を設定します。

ステージ2が終わった時点で、コネクションプールが担当する変数（`*connection-pool*`、内部の `*thread-connection-pool*`、および `initialize-table-information` を直接呼ばない限り設定されない `*table-information-initialized*` など）を**除く**すべての `clails/environment` 変数が設定済みになります。

**ステージ3 -- ランタイム起動（`call-startup-hooks`）**

`*connection-pool*` が実際に作られるのはこの段階です。`*startup-hooks*` に登録された関数を順に実行します（デフォルトは `clails/model/connection:startup-connection-pool` のみ。生成されたプロジェクトでは `app/config/environment.lisp` でこのリストの先頭に `initialize-table-information` とロガー初期化処理も push されます）。**このステージに到達するのは `clails server` のみ**です。`call-startup-hooks` はサーバーがリクエストの受付を開始する直前に、`clails/cmd:server`（`src/cmd.lisp`）から一度だけ呼び出されます。対応するシャットダウンステージ（`*shutdown-hooks*` を実行する `call-shutdown-hooks`）は `clails stop` 実行時、あるいは稼働中のサーバーが割り込まれたときにのみ到達します。

> **`db:seed` と `test` は特殊なケースです。** どちらも `call-startup-hooks` は呼びませんが、実際の処理を行う前に `clails/model/connection:startup-connection-pool`（および `initialize-table-information`）を `src/cmd.lisp` 内で直接（ハードコードされた形で）呼び出し、処理後にプールをシャットダウン（`shutdown-connection-pool`）します。そのため `db:seed`/`test` の実行中は `*connection-pool*` が**設定されています**が、プロジェクトが `*startup-hooks*` に追加したデフォルト以外のカスタムフックは、`call-startup-hooks` を経由しないためこの2つのコマンドでは実行**されません**。
>
> `db:create`、`db:migrate`、`db:migrate:up`、`db:migrate:down`、`db:rollback` はそもそもプールを必要としません。これらは短命な直接接続（`with-db-connection-direct`）でデータベースとやり取りしており、`*connection-pool*` は一切使いません。

### CLIコマンドごとに到達するステージ

| コマンド | ステージ2（プロジェクトロード）に到達するか | ステージ3（`*connection-pool*` 設定済み）に到達するか | 備考 |
|---|---|---|---|
| `new` | しない | しない | ディスク上にプロジェクトを作成するだけで、まだロードするプロジェクトが存在しない |
| `environment` | する | しない | `*project-environment*` を表示するだけ |
| `generate:model` / `:migration` / `:view` / `:controller` / `:scaffold` / `:task` | する | しない | ファイル生成のみで、データベースには一切触れない |
| `db:create` | する | しない（直接接続） | `with-db-connection-direct` を使用し、プールは使わない |
| `db:migrate`、`db:migrate:up`、`db:migrate:down`、`db:rollback`、`db:status` | する | しない（直接接続） | マイグレーションはプールではなく直接接続で実行される |
| `db:seed` | する | **する**（`call-startup-hooks` ではなく `startup-connection-pool`/`initialize-table-information` の直接呼び出し経由） | デフォルトのプール起動以外のカスタム起動フックはスキップされる。シード完了後にプールは再びシャットダウンされる |
| `test` | する（環境は強制的に `:test` になる） | **する**（`db:seed` と同様の直接呼び出しの注意点あり） | テスト実行後にプールは再びシャットダウンされる |
| `task`（`clails task ...` によるカスタムタスク） | する | しない（タスク自身が `startup-connection-pool` を呼ばない限り） | フレームワークはタスクのためにプールを起動しない。プール経由のDBアクセスが必要なタスクは自分でプールを起動する必要がある |
| `server` | する | **する**（`call-startup-hooks` 経由） | ユーザーが設定可能な `*startup-hooks*` リスト全体を実行する唯一のコマンド |
| `stop` | する | 該当なし | 同一プロセス内で稼働中のサーバーに対してのみ意味を持つ。単独で実行しても停止対象は存在しない |

### 実務上の意味

`clails/environment:*connection-pool*` を読むコード（直接、あるいは `clails/model/connection:get-connection`/`with-db-connection` 経由での間接的な参照を含む）は、プロジェクトがロードされている（ステージ2に到達している）というだけでプールが設定済みだと仮定してはいけません。これが保証されるのは `server`、`db:seed`、`test` の実行中だけです。プール経由のデータベース接続が必要なカスタムタスクやその他のコード経路を書く場合は、自分で `clails/model/connection:startup-connection-pool` を呼び出す（処理後に `shutdown-connection-pool` も呼ぶ）か、代わりに直接接続（`with-db-connection-direct`）を使ってください。`*connection-pool*` が `nil` の場合、`clails/model/connection:get-connection` はコネクションプールライブラリ内部から発生する分かりにくいエラーの代わりに、何が初期化されていないかを明示したエラーを送出します。

---

## 1. 環境変数

### データベース関連

clails アプリケーションは、データベース接続情報を環境変数で設定できます。
これらの環境変数は、`app/config/database.lisp` で参照されます。

#### SQLite3 の場合

| 環境変数名 | 説明 | デフォルト値（開発環境） | デフォルト値（テスト環境） | 本番環境での扱い |
|-----------|------|----------------------|----------------------|----------------|
| `CLAILS_DB_NAME` | データベースファイルのパス | `{project-dir}/tmp/{project-name}-develop.sqlite3` | `{project-dir}/tmp/{project-name}-test.sqlite3` | 必須（デフォルト値なし） |

**設定例**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" 
                                            "./tmp/myapp-develop.sqlite3"))
    :test (:database-name ,(env-or-default "CLAILS_DB_NAME" 
                                         "./tmp/myapp-test.sqlite3"))
    :production (:database-name ,(env "CLAILS_DB_NAME"))))
```

**コネクションプール設定（オプション）**:

SQLite3 では、コネクションプールの動作を細かく制御するための追加パラメータを指定できます。

| パラメータ名 | 説明 | 型 | デフォルト値 |
|------------|------|-----|------------|
| `:initial-size` | プールの初期サイズ（起動時に作成されるコネクション数） | integer | 10 |
| `:max-size` | プールの最大サイズ（同時接続の上限） | integer | 10 |
| `:checkout-timeout` | コネクション取得のタイムアウト（秒） | integer | 30 |
| `:idle-timeout` | アイドルコネクションの破棄時間（秒） | integer | 600 |
| `:max-lifetime` | コネクションの最大生存時間（秒） | integer | 1800 |
| `:keepalive-interval` | キープアライブの実行間隔（秒） | integer | 0（無効） |
| `:reaper-interval` | 使用されていないコネクションの回収間隔（秒） | integer | 60 |

**注意**: SQLite3 は `:validation-query` パラメータをサポートしていません。

**設定例（コネクションプールのカスタマイズ）**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" 
                                            "./tmp/myapp-develop.sqlite3")
              ;; コネクションプール設定
              :initial-size 10         ; 起動時に10個のコネクションを作成
              :max-size 10             ; 最大10接続まで許可
              :checkout-timeout 30     ; 30秒待ってもコネクションが取得できない場合はエラー
              :idle-timeout 600        ; 10分間アイドル状態のコネクションを破棄
              :max-lifetime 1800       ; コネクションの最大生存時間は30分
              :keepalive-interval 0    ; キープアライブは無効
              :reaper-interval 60)     ; 60秒ごとに不要なコネクションを回収
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 ;; 本番環境でもコネクションプール設定可能
                 :initial-size 10
                 :max-size 10
                 :checkout-timeout 30
                 :idle-timeout 600
                 :max-lifetime 1800
                 :keepalive-interval 0
                 :reaper-interval 60)))
```

#### MySQL の場合

| 環境変数名 | 説明 | デフォルト値（開発環境） | デフォルト値（テスト環境） | 本番環境での扱い |
|-----------|------|----------------------|----------------------|----------------|
| `CLAILS_DB_NAME` | データベース名 | `{project-name}_develop` | `{project-name}_test` | 必須（デフォルト値なし） |
| `CLAILS_DB_HOST` | ホスト名 | `localhost` | `localhost` | 必須（デフォルト値なし） |
| `CLAILS_DB_PORT` | ポート番号 | `3306` | `3306` | 必須（デフォルト値なし） |
| `CLAILS_DB_USERNAME` | ユーザー名 | `root` | `root` | 必須（デフォルト値なし） |
| `CLAILS_DB_PASSWORD` | パスワード | `password` | `password` | 必須（デフォルト値なし） |

**設定例**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_develop")
              :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
              :port ,(env-or-default "CLAILS_DB_PORT" "3306")
              :username ,(env-or-default "CLAILS_DB_USERNAME" "root")
              :password ,(env-or-default "CLAILS_DB_PASSWORD" "password"))
    :test (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_test")
           :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
           :port ,(env-or-default "CLAILS_DB_PORT" "3306")
           :username ,(env-or-default "CLAILS_DB_USERNAME" "root")
           :password ,(env-or-default "CLAILS_DB_PASSWORD" "password"))
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 :host ,(env "CLAILS_DB_HOST")
                 :port ,(env "CLAILS_DB_PORT")
                 :username ,(env "CLAILS_DB_USERNAME")
                 :password ,(env "CLAILS_DB_PASSWORD"))))
```

**コネクションプール設定（オプション）**:

MySQL では、コネクションプールの動作を細かく制御するための追加パラメータを指定できます。

| パラメータ名 | 説明 | 型 | デフォルト値 |
|------------|------|-----|------------|
| `:initial-size` | プールの初期サイズ（起動時に作成されるコネクション数） | integer | 3 |
| `:max-size` | プールの最大サイズ（同時接続の上限） | integer | 10 |
| `:checkout-timeout` | コネクション取得のタイムアウト（秒） | integer | 5 |
| `:idle-timeout` | アイドルコネクションの破棄時間（秒） | integer | 600 |
| `:max-lifetime` | コネクションの最大生存時間（秒） | integer | 1800 |
| `:keepalive-interval` | キープアライブの実行間隔（秒） | integer | 0（無効） |
| `:validation-query` | コネクション検証用のクエリ | string | "SELECT 1" |
| `:reaper-interval` | 使用されていないコネクションの回収間隔（秒） | integer | 60 |

**設定例（コネクションプールのカスタマイズ）**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_develop")
              :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
              :port ,(env-or-default "CLAILS_DB_PORT" "3306")
              :username ,(env-or-default "CLAILS_DB_USERNAME" "root")
              :password ,(env-or-default "CLAILS_DB_PASSWORD" "password")
              ;; コネクションプール設定
              :initial-size 5          ; 起動時に5つのコネクションを作成
              :max-size 20             ; 最大20接続まで許可
              :checkout-timeout 10     ; 10秒待ってもコネクションが取得できない場合はエラー
              :idle-timeout 300        ; 5分間アイドル状態のコネクションを破棄
              :max-lifetime 3600       ; コネクションの最大生存時間は1時間
              :keepalive-interval 30   ; 30秒ごとにキープアライブを実行
              :validation-query "SELECT 1" ; コネクション検証用クエリ
              :reaper-interval 30)     ; 30秒ごとに不要なコネクションを回収
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 :host ,(env "CLAILS_DB_HOST")
                 :port ,(env "CLAILS_DB_PORT")
                 :username ,(env "CLAILS_DB_USERNAME")
                 :password ,(env "CLAILS_DB_PASSWORD")
                 ;; 本番環境では大きめの値を設定
                 :initial-size 10
                 :max-size 50
                 :checkout-timeout 5
                 :idle-timeout 600
                 :max-lifetime 3600
                 :keepalive-interval 60
                 :reaper-interval 60)))
```

#### PostgreSQL の場合

| 環境変数名 | 説明 | デフォルト値（開発環境） | デフォルト値（テスト環境） | 本番環境での扱い |
|-----------|------|----------------------|----------------------|----------------|
| `CLAILS_DB_NAME` | データベース名 | `{project-name}_develop` | `{project-name}_test` | 必須（デフォルト値なし） |
| `CLAILS_DB_HOST` | ホスト名 | `localhost` | `localhost` | 必須（デフォルト値なし） |
| `CLAILS_DB_PORT` | ポート番号 | `5432` | `5432` | 必須（デフォルト値なし） |
| `CLAILS_DB_USERNAME` | ユーザー名 | `postgres` | `postgres` | 必須（デフォルト値なし） |
| `CLAILS_DB_PASSWORD` | パスワード | `password` | `password` | 必須（デフォルト値なし） |

**設定例**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_develop")
              :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
              :port ,(env-or-default "CLAILS_DB_PORT" "5432")
              :username ,(env-or-default "CLAILS_DB_USERNAME" "postgres")
              :password ,(env-or-default "CLAILS_DB_PASSWORD" "password"))
    :test (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_test")
           :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
           :port ,(env-or-default "CLAILS_DB_PORT" "5432")
           :username ,(env-or-default "CLAILS_DB_USERNAME" "postgres")
           :password ,(env-or-default "CLAILS_DB_PASSWORD" "password"))
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 :host ,(env "CLAILS_DB_HOST")
                 :port ,(env "CLAILS_DB_PORT")
                 :username ,(env "CLAILS_DB_USERNAME")
                 :password ,(env "CLAILS_DB_PASSWORD"))))
```

**コネクションプール設定（オプション）**:

PostgreSQL では、コネクションプールの動作を細かく制御するための追加パラメータを指定できます。

| パラメータ名 | 説明 | 型 | デフォルト値 |
|------------|------|-----|------------|
| `:initial-size` | プールの初期サイズ（起動時に作成されるコネクション数） | integer | 3 |
| `:max-size` | プールの最大サイズ（同時接続の上限） | integer | 10 |
| `:checkout-timeout` | コネクション取得のタイムアウト（秒） | integer | 5 |
| `:idle-timeout` | アイドルコネクションの破棄時間（秒） | integer | 600 |
| `:max-lifetime` | コネクションの最大生存時間（秒） | integer | 1800 |
| `:keepalive-interval` | キープアライブの実行間隔（秒） | integer | 0（無効） |
| `:validation-query` | コネクション検証用のクエリ | string | "SELECT 1" |
| `:reaper-interval` | 使用されていないコネクションの回収間隔（秒） | integer | 60 |

**設定例（コネクションプールのカスタマイズ）**:
```lisp
;; app/config/database.lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(env-or-default "CLAILS_DB_NAME" "myapp_develop")
              :host ,(env-or-default "CLAILS_DB_HOST" "localhost")
              :port ,(env-or-default "CLAILS_DB_PORT" "5432")
              :username ,(env-or-default "CLAILS_DB_USERNAME" "postgres")
              :password ,(env-or-default "CLAILS_DB_PASSWORD" "password")
              ;; コネクションプール設定
              :initial-size 5          ; 起動時に5つのコネクションを作成
              :max-size 20             ; 最大20接続まで許可
              :checkout-timeout 10     ; 10秒待ってもコネクションが取得できない場合はエラー
              :idle-timeout 300        ; 5分間アイドル状態のコネクションを破棄
              :max-lifetime 3600       ; コネクションの最大生存時間は1時間
              :keepalive-interval 30   ; 30秒ごとにキープアライブを実行
              :validation-query "SELECT 1" ; コネクション検証用クエリ
              :reaper-interval 30)     ; 30秒ごとに不要なコネクションを回収
    :production (:database-name ,(env "CLAILS_DB_NAME")
                 :host ,(env "CLAILS_DB_HOST")
                 :port ,(env "CLAILS_DB_PORT")
                 :username ,(env "CLAILS_DB_USERNAME")
                 :password ,(env "CLAILS_DB_PASSWORD")
                 ;; 本番環境では大きめの値を設定
                 :initial-size 10
                 :max-size 50
                 :checkout-timeout 5
                 :idle-timeout 600
                 :max-lifetime 3600
                 :keepalive-interval 60
                 :reaper-interval 60)))
```

### 環境変数の取得ユーティリティ

clails は環境変数を取得するためのユーティリティ関数を提供します。

#### `env` 関数

環境変数の値を取得します。環境変数が設定されていない場合は `NIL` を返します。

```lisp
(clails/util:env "CLAILS_DB_NAME")
;; => "myapp_develop" または NIL
```

#### `env-or-default` 関数

環境変数の値を取得します。環境変数が設定されていない場合はデフォルト値を返します。

```lisp
(clails/util:env-or-default "CLAILS_DB_HOST" "localhost")
;; => "localhost" (環境変数が設定されていない場合)
;; => "db.example.com" (環境変数が設定されている場合)
```

**パラメータ**:
- `env-name` [string] - 環境変数名
- `default-value` [t] - デフォルト値

**戻り値**:
- [string] - 環境変数の値
- [t] - デフォルト値（環境変数が設定されていない場合）

---

## 2. グローバル変数

clails アプリケーションは、`clails/environment` パッケージで定義されたグローバル変数を通じて設定を管理します。
これらの変数は、`app/config/environment.lisp` や `app/config/database.lisp` で設定します。

### プロジェクト関連

#### `*project-name*`

プロジェクトの名前を指定します。

**型**: string

**デフォルト値**: `""`

**設定場所**: `app/config/environment.lisp`

**設定例**:
```lisp
(setf clails/environment:*project-name* "my-application")
```

#### `*project-dir*`

プロジェクトのディレクトリパスを指定します。

**型**: string

**デフォルト値**: `""`

**設定場所**: アプリケーション起動時に自動設定

**参照例**:
```lisp
(format t "Project directory: ~A~%" clails/environment:*project-dir*)
```

#### `*project-environment*`

実行環境（開発、テスト、本番）を指定します。

**型**: keyword

**デフォルト値**: `:develop`

**設定可能な値**:
- `:develop` - 開発環境
- `:test` - テスト環境
- `:production` - 本番環境

**設定場所**: `app/config/environment.lisp`

**設定例**:
```lisp
;; 環境変数から設定
(clails/environment:set-environment 
  (clails/util:env-or-default "APP_ENV" "DEVELOP"))

;; または直接設定
(setf clails/environment:*project-environment* :production)
```

### データベース関連

#### `*database-config*`

データベース接続情報を保持します。

**型**: plist

**デフォルト値**: `NIL`

**設定場所**: `app/config/database.lisp`

**設定例**:
```lisp
(setf clails/environment:*database-config*
  (list
    :develop (:database-name "myapp_develop"
              :host "localhost"
              :port "5432"
              :username "postgres"
              :password "password")
    :test (:database-name "myapp_test"
           :host "localhost"
           :port "5432"
           :username "postgres"
           :password "password")
    :production (:database-name "myapp_production"
                 :host "db.example.com"
                 :port "5432"
                 :username "app_user"
                 :password "secret")))
```

#### `*database-type*`

使用するデータベースの種類を指定します。

**型**: `<database-type>` のサブクラスのインスタンス

**デフォルト値**: `NIL`

**設定可能な値**:
- `(make-instance 'clails/environment:<database-type-mysql>)` - MySQL
- `(make-instance 'clails/environment:<database-type-postgresql>)` - PostgreSQL
- `(make-instance 'clails/environment:<database-type-sqlite3>)` - SQLite3

**設定場所**: `app/config/database.lisp`

**設定例**:
```lisp
;; PostgreSQL を使用
(setf clails/environment:*database-type*
      (make-instance 'clails/environment:<database-type-postgresql>))

;; MySQL を使用
(setf clails/environment:*database-type*
      (make-instance 'clails/environment:<database-type-mysql>))

;; SQLite3 を使用
(setf clails/environment:*database-type*
      (make-instance 'clails/environment:<database-type-sqlite3>))
```

#### `*migration-base-dir*`

マイグレーションファイルを配置するディレクトリのベースパスを指定します。

**型**: string

**デフォルト値**: `""`

**設定場所**: 通常は `*project-dir*` と同じ値を使用（テスト時などに変更可能）

**設定例**:
```lisp
(setf clails/environment:*migration-base-dir* 
      clails/environment:*project-dir*)
```

#### `*connection-pool*`

データベースコネクションプールを保持します。

**型**: connection-pool object

**デフォルト値**: `NIL`

**設定場所**: アプリケーション起動時に自動作成、シャットダウン時に破棄

**注意**: この変数は直接操作しないでください。

### ルーティング関連

#### `*routing-tables*`

URL パスと Controller の対応を定義するルーティングテーブルです。

**型**: list

**デフォルト値**: `'((:path "/" :controller "clails/controller/base-controller:<default-controller>"))`

**設定場所**: `app/config/environment.lisp`

**ルートエントリのプロパティ**:

各ルートエントリは以下のプロパティを持つplistです：

**必須プロパティ**:
- `:path` [string] - URI パスパターン。 `/users/:id` のようなパラメータプレースホルダーをサポート
- `:controller` [string] - `"package::<class-name>"` 形式の完全修飾コントローラークラス名

**オプションプロパティ** (カスタムルーティングパターン用):
- `:scanner` [string] - リクエストパスにマッチするカスタム正規表現パターン文字列。最優先で使用される
- `:keys` [list of strings] - 抽出するURLパラメータ名のリスト。`:scanner`と併せて使用
- `:generate-scanner` [function designator] - `:scanner`と`:keys`を動的に生成する関数。`:scanner` (string)と`:keys` (list)を含むplistを返す必要がある

**スキャナー生成の優先順位**:
1. `:scanner` (最優先)
2. `:generate-scanner` (`:scanner`が指定されていない場合のみ)
3. デフォルト動作（`create-scanner-from-uri-path`を使用）

**基本的な使い方**:
```lisp
;; app/config/environment.lisp
(setf clails/environment:*routing-tables*
  '(;; トップページ
    (:path "/"
     :controller "myapp/controller::<top-controller>")
    
    ;; ユーザー一覧・作成
    (:path "/users"
     :controller "myapp/controller::<users-controller>")
    
    ;; ユーザー詳細・更新・削除
    (:path "/users/:id"
     :controller "myapp/controller::<user-controller>")
    
    ;; ネストしたリソース
    (:path "/api/posts/:post-id/comments/:comment-id"
     :controller "myapp/controller/api::<comments-controller>")))

;; ルーティングテーブルの初期化
(clails/controller/base-controller:initialize-routing-tables)
```

**高度な使い方 - カスタムルーティングパターン**:

```lisp
;; SPA（Single Page Application）用のキャッチオールルート
(setf clails/environment:*routing-tables*
  '((:path "/spa/*"
     :controller "myapp/controller::<spa-controller>"
     :scanner "^/spa/.*$")))

;; パラメータ抽出を伴う静的ファイル配信
(setf clails/environment:*routing-tables*
  '((:path "/static/*"
     :controller "myapp/controller::<static-controller>"
     :scanner "^/static/(.*)$"
     :keys ("filepath"))))

;; 数値IDのみの制約
(setf clails/environment:*routing-tables*
  '((:path "/users/:id"
     :controller "myapp/controller::<user-controller>"
     :scanner "^/users/([0-9]+)$"
     :keys ("id"))))

;; カスタムスキャナー生成関数
(setf clails/environment:*routing-tables*
  '((:path "/api/*"
     :controller "myapp/controller::<api-controller>"
     :generate-scanner (lambda (route-entry)
                         (let ((path (getf route-entry :path)))
                           (list :scanner "^/api/.*$"
                                 :keys nil))))))

;; 混合パターン
(setf clails/environment:*routing-tables*
  '(;; パラメータを持つデフォルトパターン
    (:path "/posts/:post-id/comments/:comment-id"
     :controller "myapp/controller::<comments-controller>")
    
    ;; SPA用のキャッチオール
    (:path "/app/*"
     :controller "myapp/controller::<spa-controller>"
     :scanner "^/app/.*$")
    
    ;; パラメータを持つカスタムパターン
    (:path "/files/*"
     :controller "myapp/controller::<file-controller>"
     :scanner "^/files/(.*)$"
     :keys ("path"))))

;; ルーティングテーブルの初期化
(clails/controller/base-controller:initialize-routing-tables)
```

### アプリケーションライフサイクル関連

#### `*startup-hooks*`

アプリケーション起動時に実行される関数のリストです。

**型**: list of strings or symbols

**デフォルト値**: `'("clails/model/connection:startup-connection-pool")`

**設定場所**: `app/config/environment.lisp`

**指定方法**:
- 文字列: `"package-name:function-name"` の形式で指定
- シンボル: 関数名のシンボルで指定

循環参照が発生する場合は、文字列で指定してください。

**設定例**:
```lisp
;; 文字列で指定（推奨: 循環参照を避けるため）
(setf clails/environment:*startup-hooks*
  '("clails/model/connection:startup-connection-pool"
    "myapp/initializer:setup-logger"
    "myapp/initializer:load-cache"))

;; シンボルで指定（循環参照がない場合のみ）
(setf clails/environment:*startup-hooks*
  '(clails/model/connection:startup-connection-pool
    myapp/initializer:setup-logger
    myapp/initializer:load-cache))

;; 混在も可能
(setf clails/environment:*startup-hooks*
  '("clails/model/connection:startup-connection-pool"
    myapp/initializer:setup-logger
    "myapp/initializer:load-cache"))
```

#### `*shutdown-hooks*`

アプリケーション終了時に実行される関数のリストです。

**型**: list of strings or symbols

**デフォルト値**: `'("clails/model/connection:shutdown-connection-pool")`

**設定場所**: `app/config/environment.lisp`

**指定方法**:
- 文字列: `"package-name:function-name"` の形式で指定
- シンボル: 関数名のシンボルで指定

循環参照が発生する場合は、文字列で指定してください。

**設定例**:
```lisp
;; 文字列で指定（推奨: 循環参照を避けるため）
(setf clails/environment:*shutdown-hooks*
  '("clails/model/connection:shutdown-connection-pool"
    "myapp/finalizer:cleanup-cache"
    "myapp/finalizer:save-statistics"))

;; シンボルで指定（循環参照がない場合のみ）
(setf clails/environment:*shutdown-hooks*
  '(clails/model/connection:shutdown-connection-pool
    myapp/finalizer:cleanup-cache
    myapp/finalizer:save-statistics))

;; 混在も可能
(setf clails/environment:*shutdown-hooks*
  '("clails/model/connection:shutdown-connection-pool"
    myapp/finalizer:cleanup-cache
    "myapp/finalizer:save-statistics"))
```

---

## 3. Middleware の設定

clails は Lack middleware をサポートしており、リクエスト処理のパイプラインをカスタマイズできます。

### Middleware スタック

#### `*clails-middleware-stack*`

Lack middleware のリストを保持します。リクエストはこのスタックの順序で処理されます。

**パッケージ**: `clails/middleware`

**型**: list of middleware functions

**デフォルト値**: 
```lisp
(list
  *lack-middleware-transaction*
  *lack-middleware-clails-controller*
  #'(lambda (app)
      (funcall *lack-middleware-static*
               app
               :path "/"
               :root #P"./public/")))
```

**設定場所**: `app/config/environment.lisp`

**注意**: この変数は直接変更せず、`add-middleware-before` または `add-middleware-after` 関数を使用してください。

### Middleware の追加

#### `add-middleware-before` 関数

Middleware スタックの先頭に middleware を追加します。先頭に追加された middleware は、既存のすべての middleware より先に実行されます。

**パラメータ**:
- `middleware` [function] - 追加する middleware 関数

**設定例**:
```lisp
(in-package #:myapp/config/environment)

;; Lack の session middleware を追加
(clails/middleware:add-middleware-before
  (lambda (app)
    (funcall lack.middleware.session:*lack-middleware-session*
             app
             :state (make-instance 'lack.session.state.cookie:cookie-state))))

;; カスタム middleware を追加
(clails/middleware:add-middleware-before
  (lambda (app)
    (lambda (env)
      ;; リクエスト前の処理
      (format t "Request started: ~A~%" (getf env :path-info))
      (let ((response (funcall app env)))
        ;; レスポンス後の処理
        (format t "Request completed~%")
        response))))
```

#### `add-middleware-after` 関数

Middleware スタックの末尾に middleware を追加します。末尾に追加された middleware は、既存のすべての middleware より後に実行されます。

**パラメータ**:
- `middleware` [function] - 追加する middleware 関数

**設定例**:
```lisp
(in-package #:myapp/config/environment)

;; ログ出力 middleware を追加
(clails/middleware:add-middleware-after
  (lambda (app)
    (lambda (env)
      (let* ((start-time (get-internal-real-time))
             (response (funcall app env))
             (elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        (format t "Request time: ~A seconds~%" elapsed)
        response))))
```

### 組み込み Middleware

#### `*lack-middleware-transaction*`

データベーストランザクションを自動管理する middleware です。

**パッケージ**: `clails/middleware/transaction-middleware`

**機能**:
- リクエストごとにデータベース接続を取得
- トランザクションを開始
- リクエスト処理が正常に完了すればコミット
- エラーが発生すればロールバック
- 接続をコネクションプールに返却

**有効/無効の切り替え**:
```lisp
;; トランザクション middleware を無効化
(setf clails/middleware/transaction-middleware:*enable-transaction-middleware* nil)

;; トランザクション middleware を有効化（デフォルト）
(setf clails/middleware/transaction-middleware:*enable-transaction-middleware* t)
```

#### `*lack-middleware-clails-controller*`

ルーティングと Controller のディスパッチを行う middleware です。

**パッケージ**: `clails/middleware/clails-middleware`

**機能**:
- URL パスから Controller を検索
- HTTP メソッド（GET/POST/PUT/DELETE）に応じて適切なメソッドを呼び出し
- View の解決とレンダリング
- 404 エラーのハンドリング

**注意**: この middleware は必須です。削除しないでください。

#### `*lack-middleware-static*`

静的ファイル（CSS、JavaScript、画像など）を配信する middleware です。

**パッケージ**: `lack.middleware.static`

**デフォルト設定**:
```lisp
#'(lambda (app)
    (funcall *lack-middleware-static*
             app
             :path "/"
             :root #P"./public/"))
```

**カスタマイズ例**:
```lisp
;; 静的ファイルのパスを変更
(setf clails/middleware:*clails-middleware-stack*
  (list
    clails/middleware:*lack-middleware-transaction*
    clails/middleware:*lack-middleware-clails-controller*
    #'(lambda (app)
        (funcall lack.middleware.static:*lack-middleware-static*
                 app
                 :path "/static"
                 :root #P"./assets/"))))
```

### Middleware の実行順序

Middleware は `*clails-middleware-stack*` の順序で実行されます。

```
リクエスト
  ↓
*lack-middleware-transaction*
  ↓
*lack-middleware-clails-controller*
  ↓
*lack-middleware-static*
  ↓
レスポンス
```

### Middleware の使用例

#### セッション管理

```lisp
(in-package #:myapp/config/environment)

;; Lack の session middleware を追加
(clails/middleware:add-middleware-before
  (lambda (app)
    (funcall lack.middleware.session:*lack-middleware-session*
             app
             :state (make-instance 'lack.session.state.cookie:cookie-state
                                  :secret "your-secret-key"
                                  :httponly t))))
```

#### CORS 対応

```lisp
(in-package #:myapp/config/environment)

;; CORS middleware を追加
(clails/middleware:add-middleware-before
  (lambda (app)
    (lambda (env)
      (let ((response (funcall app env)))
        ;; CORS ヘッダーを追加
        (setf (getf (second response) :access-control-allow-origin) "*")
        (setf (getf (second response) :access-control-allow-methods) "GET, POST, PUT, DELETE")
        response))))
```

#### リクエストログ

```lisp
(in-package #:myapp/config/environment)

;; リクエストログ middleware を追加
(clails/middleware:add-middleware-before
  (lambda (app)
    (lambda (env)
      (format t "~A ~A~%"
              (getf env :request-method)
              (getf env :path-info))
      (funcall app env))))
```

#### 認証

```lisp
(in-package #:myapp/config/environment)

;; 認証 middleware を追加
(clails/middleware:add-middleware-before
  (lambda (app)
    (lambda (env)
      (let ((path (getf env :path-info)))
        ;; 特定のパスは認証をスキップ
        (if (or (string= path "/login")
                (string= path "/public"))
            (funcall app env)
            ;; 認証チェック
            (if (authenticated-p env)
                (funcall app env)
                '(401 (:content-type "text/plain") ("Unauthorized"))))))))
```

#### Middleware スタックの確認

```lisp
;; 現在の middleware スタックを表示
(clails/middleware:show-middleware-stack)
```

---

## 4. 環境変数の設定方法

### 開発環境での設定

開発環境では、シェルで環境変数を設定するか、`.env` ファイルを使用できます。

#### シェルで設定

```bash
# Bash/Zsh
export CLAILS_DB_NAME="myapp_develop"
export CLAILS_DB_HOST="localhost"
export CLAILS_DB_PORT="5432"
export CLAILS_DB_USERNAME="postgres"
export CLAILS_DB_PASSWORD="password"

# アプリケーション起動
clails server
```

#### .env ファイルを使用（direnv など）

```bash
# .env
export CLAILS_DB_NAME="myapp_develop"
export CLAILS_DB_HOST="localhost"
export CLAILS_DB_PORT="5432"
export CLAILS_DB_USERNAME="postgres"
export CLAILS_DB_PASSWORD="password"
```

### 本番環境での設定

本番環境では、環境変数を必ず設定してください。デフォルト値に依存しないでください。

```bash
# Systemd サービスの場合
[Service]
Environment="CLAILS_DB_NAME=myapp_production"
Environment="CLAILS_DB_HOST=db.example.com"
Environment="CLAILS_DB_PORT=5432"
Environment="CLAILS_DB_USERNAME=app_user"
Environment="CLAILS_DB_PASSWORD=secret_password"

# Docker Compose の場合
services:
  app:
    environment:
      - CLAILS_DB_NAME=myapp_production
      - CLAILS_DB_HOST=db
      - CLAILS_DB_PORT=5432
      - CLAILS_DB_USERNAME=app_user
      - CLAILS_DB_PASSWORD=secret_password
```

### テスト環境での設定

テスト環境では、テスト用の設定を使用します。

```bash
# テスト実行時
export CLAILS_DB_NAME="myapp_test"
export APP_ENV="TEST"

# テスト実行
qlot exec rove myapp-test.asd
```

---

## 5. 設定ファイルの例

### app/config/environment.lisp

```lisp
(in-package #:myapp/config)

;; プロジェクト名の設定
(setf clails/environment:*project-name* "myapp")

;; 実行環境の設定
(clails/environment:set-environment 
  (clails/util:env-or-default "APP_ENV" "DEVELOP"))

;; スタートアップフックの設定
(setf clails/environment:*startup-hooks*
  '("clails/model/connection:startup-connection-pool"
    "myapp/initializer:initialize-table-information"
    "myapp/initializer:setup-logger"))

;; シャットダウンフックの設定
(setf clails/environment:*shutdown-hooks*
  '("myapp/finalizer:cleanup-resources"
    "clails/model/connection:shutdown-connection-pool"))
```

### app/config/database.lisp

```lisp
(in-package #:myapp/config)

;; データベースタイプの設定
(setf clails/environment:*database-type*
      (make-instance 'clails/environment:<database-type-postgresql>))

;; データベース接続情報の設定
(setf clails/environment:*database-config*
  (list
    :develop (:database-name ,(clails/util:env-or-default 
                               "CLAILS_DB_NAME" "myapp_develop")
              :host ,(clails/util:env-or-default 
                      "CLAILS_DB_HOST" "localhost")
              :port ,(clails/util:env-or-default 
                      "CLAILS_DB_PORT" "5432")
              :username ,(clails/util:env-or-default 
                          "CLAILS_DB_USERNAME" "postgres")
              :password ,(clails/util:env-or-default 
                          "CLAILS_DB_PASSWORD" "password"))
    :test (:database-name ,(clails/util:env-or-default 
                            "CLAILS_DB_NAME" "myapp_test")
           :host ,(clails/util:env-or-default 
                   "CLAILS_DB_HOST" "localhost")
           :port ,(clails/util:env-or-default 
                   "CLAILS_DB_PORT" "5432")
           :username ,(clails/util:env-or-default 
                       "CLAILS_DB_USERNAME" "postgres")
           :password ,(clails/util:env-or-default 
                       "CLAILS_DB_PASSWORD" "password"))
    :production (:database-name ,(clails/util:env "CLAILS_DB_NAME")
                 :host ,(clails/util:env "CLAILS_DB_HOST")
                 :port ,(clails/util:env "CLAILS_DB_PORT")
                 :username ,(clails/util:env "CLAILS_DB_USERNAME")
                 :password ,(clails/util:env "CLAILS_DB_PASSWORD"))))
```

### app/config/routes.lisp

```lisp
(in-package #:myapp/config)

;; ルーティングテーブルの設定
(setf clails/environment:*routing-tables*
  '((:path "/"
     :controller "myapp/controller::<top-controller>")
    
    (:path "/users"
     :controller "myapp/controller::<users-controller>")
    
    (:path "/users/:id"
     :controller "myapp/controller::<user-controller>")
    
    (:path "/api/posts"
     :controller "myapp/controller/api::<posts-controller>")
    
    (:path "/api/posts/:id"
     :controller "myapp/controller/api::<post-controller>")))

;; ルーティングテーブルの初期化
(clails/controller/base-controller:initialize-routing-tables)
```

---

## 6. ベストプラクティス

### 環境変数の使用

1. **開発環境**: デフォルト値を使用して手軽に開発できるようにする
2. **テスト環境**: テスト専用の設定を使用する
3. **本番環境**: 環境変数を必須にし、デフォルト値に依存しない

### セキュリティ

1. **パスワードや機密情報**: 環境変数で管理し、ソースコードにハードコードしない
2. **本番環境の設定**: `.env` ファイルをバージョン管理に含めない
3. **環境変数の検証**: 起動時に必要な環境変数が設定されているかチェックする

```lisp
;; 本番環境での環境変数チェック例
(when (eq clails/environment:*project-environment* :production)
  (unless (clails/util:env "CLAILS_DB_PASSWORD")
    (error "CLAILS_DB_PASSWORD is required in production")))
```

### 設定の分離

1. **環境ごとに異なる設定**: 環境変数を使用
2. **環境に依存しない設定**: 設定ファイルに直接記述
3. **複雑な設定**: 専用の初期化関数を作成

---

## 7. トラブルシューティング

### 環境変数が反映されない

**原因**: 環境変数の設定タイミングが遅い、または設定方法が間違っている

**解決方法**:
```bash
# 環境変数を設定してからアプリケーションを起動
export CLAILS_DB_NAME="myapp"
clails serer

# または同時に設定
CLAILS_DB_NAME="myapp" clails server
```

### データベースに接続できない

**原因**: データベース接続情報が正しく設定されていない

**解決方法**:
```lisp
;; 接続情報を確認
(format t "Database config: ~A~%" 
        (getf clails/environment:*database-config* 
              clails/environment:*project-environment*))

;; 環境変数を確認
(format t "DB_NAME: ~A~%" (clails/util:env "CLAILS_DB_NAME"))
```

### 本番環境で起動しない

**原因**: 必須の環境変数が設定されていない

**解決方法**:
```lisp
;; 起動時にチェックを追加
(when (eq clails/environment:*project-environment* :production)
  (let ((required-vars '("CLAILS_DB_NAME" 
                         "CLAILS_DB_HOST" 
                         "CLAILS_DB_USERNAME" 
                         "CLAILS_DB_PASSWORD")))
    (dolist (var required-vars)
      (unless (clails/util:env var)
        (error "Required environment variable ~A is not set" var)))))
```

---

## まとめ

clails の環境設定は以下の特徴を持ちます:

1. **環境変数サポート**: データベース接続情報などを環境変数で管理
2. **柔軟な設定**: 開発、テスト、本番で異なる設定を使用可能
3. **グローバル変数**: アプリケーション全体で共有される設定
4. **ライフサイクル管理**: スタートアップ/シャットダウンフックによる初期化・終了処理

適切な環境変数の設定により、安全で保守性の高いアプリケーションを構築できます。
