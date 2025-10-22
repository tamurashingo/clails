# clails コマンドガイド

## 概要

clails コマンドは、clails アプリケーションの開発を支援するコマンドラインツールです。
プロジェクトの作成、コード生成、データベース管理、サーバーの起動など、開発に必要な機能を提供します。

## 基本概念

- clails コマンドは `roswell/clails.ros` で定義されています
- 各コマンドは `src/cmd.lisp` の関数を呼び出します
- Roswell スクリプトとして実装されており、どこからでも実行できます
- プロジェクト固有のコマンド（generate, server など）はプロジェクトルートで実行します

---

## インストールとセットアップ

### Roswell のインストール

clails を使用するには、まず Roswell をインストールする必要があります。

```bash
# macOS (Homebrew)
brew install roswell

# Linux (手動インストール)
# https://github.com/roswell/roswell/wiki/Installation を参照
```

### clails のインストール

```bash
# clails コマンドのインストール
ros install tamurashingo/clails

# インストール確認
clails help
```

---

## コマンド一覧

### プロジェクト管理

| コマンド | 説明 |
|---------|------|
| `clails new` | 新しいプロジェクトを作成 |
| `clails environment` | 現在の環境を表示 |
| `clails server` | Web サーバーを起動 |
| `clails stop` | Web サーバーを停止 |

### コード生成

| コマンド | 説明 |
|---------|------|
| `clails generate:model` | Model ファイルを生成 |
| `clails generate:migration` | Migration ファイルを生成 |
| `clails generate:view` | View ファイルを生成 |
| `clails generate:controller` | Controller ファイルを生成 |
| `clails generate:scaffold` | Model、View、Controller を一括生成 |

### データベース

| コマンド | 説明 |
|---------|------|
| `clails db:create` | データベースを作成 |
| `clails db:migrate` | Migration を実行 |
| `clails db:status` | Migration の状態を表示 |

---

## 1. プロジェクト管理コマンド

### `clails new` - 新しいプロジェクトを作成

新しい clails プロジェクトを作成します。

#### 書式

```bash
clails new PROJECT_NAME [OPTIONS]
```

#### オプション

| オプション | 短縮形 | デフォルト | 説明 |
|-----------|--------|-----------|------|
| `--path PATH` | `-p PATH` | カレントディレクトリ | プロジェクトを作成するディレクトリ |
| `--database DB` | `-d DB` | `sqlite3` | 使用するデータベース（sqlite3, mysql, postgresql） |

#### 使用例

```bash
# デフォルト（SQLite3）でプロジェクト作成
clails new myapp

# MySQL を使用するプロジェクト作成
clails new myapp -d mysql

# 特定のディレクトリにプロジェクト作成
clails new myapp -p /path/to/projects

# PostgreSQL を使用
clails new myapp --database postgresql
```

#### 生成されるディレクトリ構造

```
myapp/
├── app/
│   ├── application.lisp
│   ├── controllers/
│   ├── models/
│   └── views/
├── config/
│   ├── database.lisp
│   └── routes.lisp
├── db/
│   └── migrate/
├── clails.boot
├── myapp.asd
└── README.md
```

### `clails environment` - 現在の環境を表示

現在のプロジェクト環境（development、test、production）を表示します。

#### 書式

```bash
clails environment
```

#### 使用例

```bash
cd myapp
clails environment
# => environment: DEVELOPMENT
```

### `clails server` - Web サーバーを起動

開発用 Web サーバーを起動します。

#### 書式

```bash
clails server [OPTIONS]
```

#### オプション

| オプション | 短縮形 | デフォルト | 説明 |
|-----------|--------|-----------|------|
| `--port PORT` | `-p PORT` | `5000` | サーバーのポート番号 |
| `--bind ADDRESS` | `-b ADDRESS` | `127.0.0.1` | バインドするIPアドレス |

#### 使用例

```bash
# デフォルト（localhost:5000）でサーバー起動
clails server

# ポート番号を指定
clails server -p 8080

# すべてのインターフェースでリッスン
clails server -b 0.0.0.0

# ポートとアドレスを両方指定
clails server -p 8080 -b 0.0.0.0
```

#### サーバー起動時の動作

1. ルーティングテーブルの初期化
2. ミドルウェアスタックの構築
3. Clack サーバーの起動
4. スタートアップフックの実行

#### 停止方法

```bash
# 別のターミナルで
clails stop

# または Ctrl+C
```

### `clails stop` - Web サーバーを停止

実行中の Web サーバーを停止します。

#### 書式

```bash
clails stop
```

#### 使用例

```bash
clails stop
```

---

## 2. コード生成コマンド

### `clails generate:model` - Model ファイルを生成

Model ファイルと、オプションで Migration ファイルを生成します。

#### 書式

```bash
clails generate:model MODEL_NAME [OPTIONS]
```

#### オプション

| オプション | 短縮形 | 説明 |
|-----------|--------|------|
| `--no-overwrite` | なし | 既存ファイルを上書きしない（デフォルト：有効） |
| `--no-migration` | `-n` | Migration ファイルを生成しない |

#### 使用例

```bash
# Model と Migration を生成
clails generate:model user

# Migration なしで Model のみ生成
clails generate:model user -n

# 既存ファイルを上書きして生成
clails generate:model user --no-overwrite=false
```

#### 生成されるファイル

```
app/models/user.lisp          # Model ファイル
db/migrate/YYYYMMDD-HHMMSS-user.lisp  # Migration ファイル（-n なしの場合）
```

#### 生成される Model の例

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/models/user
  (:use #:cl)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel))

(in-package #:myapp/models/user)

(defmodel <user> (<base-model>)
  (:table "users"))
```

### `clails generate:migration` - Migration ファイルを生成

データベース Migration ファイルを生成します。

#### 書式

```bash
clails generate:migration MIGRATION_NAME
```

#### 使用例

```bash
# テーブル作成用 Migration
clails generate:migration create-users-table

# カラム追加用 Migration
clails generate:migration add-email-to-users

# インデックス追加用 Migration
clails generate:migration add-index-to-users-email
```

#### 生成されるファイル

```
db/migrate/YYYYMMDD-HHMMSS-MIGRATION_NAME.lisp
```

例：`db/migrate/20241022-143000-create-users-table.lisp`

#### 生成される Migration の例

```common-lisp
(in-package #:myapp/db/migrate)

(defmigration "20241022-143000-create-users-table"
  (:up #'(lambda (conn)
           ;; ここにテーブル作成処理を記述
           ))
  (:down #'(lambda (conn)
             ;; ここにロールバック処理を記述
             )))
```

### `clails generate:view` - View ファイルを生成

View テンプレートファイルと `package.lisp` を生成します。

#### 書式

```bash
clails generate:view VIEW_NAME [OPTIONS]
```

#### オプション

| オプション | 短縮形 | 説明 |
|-----------|--------|------|
| `--no-overwrite` | なし | 既存ファイルを上書きしない（デフォルト：有効） |

#### 使用例

```bash
# トップレベルの View を生成
clails generate:view index

# ネストした View を生成
clails generate:view users/index

# 複数階層の View を生成
clails generate:view admin/users/list
```

#### 生成されるファイル

```
app/views/VIEW_NAME.html       # View テンプレート
app/views/package.lisp         # パッケージ定義（トップレベル）
app/views/DIRECTORY/package.lisp  # パッケージ定義（ディレクトリごと）
```

#### 生成される View の例

`app/views/users/index.html`:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Users Index</title>
</head>
<body>
  <h1>Users Index</h1>
</body>
</html>
```

`app/views/users/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/views/users/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view))

(in-package #:myapp/views/users/package)
```

### `clails generate:controller` - Controller ファイルを生成

Controller ファイルを生成します。

#### 書式

```bash
clails generate:controller CONTROLLER_NAME [OPTIONS]
```

#### オプション

| オプション | 短縮形 | 説明 |
|-----------|--------|------|
| `--no-overwrite` | なし | 既存ファイルを上書きしない（デフォルト：有効） |

#### 使用例

```bash
# Controller を生成
clails generate:controller users

# ネストした Controller を生成
clails generate:controller admin/users
```

#### 生成されるファイル

```
app/controllers/CONTROLLER_NAME_controller.lisp
```

#### 生成される Controller の例

```common-lisp
(in-package #:cl-user)
(defpackage #:myapp/controllers/users-controller
  (:use #:cl)
  (:import-from #:clails/controller/base-controller
                #:<web-controller>
                #:do-get
                #:do-post
                #:do-put
                #:do-delete
                #:set-view
                #:set-redirect
                #:param))

(in-package #:myapp/controllers/users-controller)

(defclass <users-controller> (<web-controller>)
  ()
  (:documentation "Users controller"))

(defmethod do-get ((controller <users-controller>))
  ;; GET リクエストの処理
  (set-view controller "users/index.html" '()))

(defmethod do-post ((controller <users-controller>))
  ;; POST リクエストの処理
  (set-redirect controller "/users"))
```

### `clails generate:scaffold` - Scaffold を生成

Model、View、Controller を一括で生成します。

#### 書式

```bash
clails generate:scaffold NAME [OPTIONS]
```

#### オプション

| オプション | 短縮形 | 説明 |
|-----------|--------|------|
| `--no-overwrite` | なし | 既存ファイルを上書きしない（デフォルト：有効） |

#### 使用例

```bash
# User の scaffold を生成
clails generate:scaffold user

# 複雑なリソースの scaffold を生成
clails generate:scaffold blog/post
```

#### 生成されるファイル

```
app/models/NAME.lisp
app/controllers/NAME_controller.lisp
app/views/NAME/index.html
app/views/NAME/show.html
app/views/NAME/new.html
app/views/NAME/edit.html
app/views/NAME/package.lisp
db/migrate/YYYYMMDD-HHMMSS-NAME.lisp
```

---

## 3. データベースコマンド

### `clails db:create` - データベースを作成

設定ファイルに基づいてデータベースを作成します。

#### 書式

```bash
clails db:create
```

#### 使用例

```bash
clails db:create
# => Creating database...
# => Database created successfully
```

#### 動作

`config/database.lisp` の設定に基づいて、以下のデータベースを作成します：

- SQLite3: データベースファイルを作成
- MySQL: `CREATE DATABASE` を実行
- PostgreSQL: `CREATE DATABASE` を実行

### `clails db:migrate` - Migration を実行

未実行の Migration を実行します。

#### 書式

```bash
clails db:migrate
```

#### エイリアス

```bash
clails db:migrate:up
```

#### 使用例

```bash
clails db:migrate
# => Running migration: 20241022-143000-create-users-table
# => Migration completed successfully
```

#### 動作

1. `db/migrate/` ディレクトリの Migration ファイルをスキャン
2. 未実行の Migration を実行順にソート
3. 各 Migration の `:up` 関数を実行
4. Migration テーブルに実行記録を保存

### `clails db:status` - Migration の状態を表示

Migration の実行状態を表示します。

#### 書式

```bash
clails db:status
```

#### 使用例

```bash
clails db:status
# => Migration Status:
# => [X] 20241022-143000-create-users-table
# => [X] 20241022-144500-add-email-to-users
# => [ ] 20241022-150000-add-index-to-users-email
```

#### 出力形式

- `[X]` - 実行済み
- `[ ]` - 未実行

---

## 4. よくある使用パターン

### 新規プロジェクトの開始

```bash
# 1. プロジェクト作成
clails new myapp -d postgresql
cd myapp

# 2. データベース作成
clails db:create

# 3. 初期 Migration 実行
clails db:migrate

# 4. サーバー起動
clails server
```

### Model を使った開発

```bash
# 1. Model と Migration を生成
clails generate:model user

# 2. Migration ファイルを編集してカラムを定義
# db/migrate/YYYYMMDD-HHMMSS-user.lisp を編集

# 3. Migration を実行
clails db:migrate

# 4. Model を使ったコードを実装
```

### REST API の作成

```bash
# 1. Controller を生成
clails generate:controller api/users

# 2. Controller に REST API のロジックを実装
# app/controllers/api/users_controller.lisp を編集

# 3. ルーティングを設定
# config/routes.lisp を編集

# 4. サーバーを起動してテスト
clails server
```

### Scaffold を使った迅速な開発

```bash
# 1. Scaffold を生成
clails generate:scaffold post

# 2. Migration を実行
clails db:migrate

# 3. 生成されたファイルをカスタマイズ
# app/models/post.lisp
# app/controllers/post_controller.lisp
# app/views/post/*.html

# 4. サーバーを起動
clails server
```

---

## 5. コマンドのオプション

### 共通オプション

多くのコマンドで共通して使用できるオプションです。

#### `--no-overwrite`

既存のファイルを上書きしないようにします。デフォルトで有効です。

```bash
# 既存ファイルがあっても上書きしない（デフォルト）
clails generate:model user

# 明示的に指定
clails generate:model user --no-overwrite

# 上書きを許可する場合（注意: データが失われる可能性があります）
# 現在、上書きを強制するオプションはありません
```

---

## 6. トラブルシューティング

### コマンドが見つからない

```bash
# clails コマンドが見つからない場合
# Roswell のパスを確認
echo $PATH

# Roswell のインストール先を確認
which ros

# clails を再インストール
ros install tamurashingo/clails
```

### プロジェクトが読み込めない

```bash
# プロジェクトルートにいることを確認
pwd

# clails.boot ファイルがあることを確認
ls clails.boot

# 依存関係を再インストール
rm -rf .qlot
qlot install
```

### Migration が実行できない

```bash
# データベース接続を確認
# config/database.lisp の設定を確認

# データベースが作成されているか確認
clails db:create

# Migration ファイルの構文エラーを確認
# db/migrate/*.lisp のファイルを確認
```

### サーバーが起動しない

```bash
# ポートが使用中でないか確認
lsof -i :5000

# 別のポートで起動
clails server -p 8080

# エラーログを確認
# ターミナルに出力されるエラーメッセージを確認
```

---

## 7. 高度な使用方法

### カスタムコマンドの追加

clails コマンドは拡張可能です。`roswell/clails.ros` を編集することで、
独自のコマンドを追加できます。

```common-lisp
;; roswell/clails.ros に追加
(defun custom/command ()
  (load-project)
  (clails/cmd:custom/command))

;; *config* に追加
(:command "custom:command"
 :function ,#'custom/command)
```

### スタートアップ・シャットダウンフック

サーバー起動時・停止時に任意の処理を実行できます。

```common-lisp
;; config/environment.lisp などで定義
(setf clails/environment:*startup-hooks*
      (list #'(lambda ()
                (format t "Server starting...~%"))))

(setf clails/environment:*shutdown-hooks*
      (list #'(lambda ()
                (format t "Server stopping...~%"))))
```

---

## まとめ

clails コマンドは以下の特徴を持ちます：

1. **統一されたインターフェース**: すべての操作を `clails` コマンドで実行
2. **自動生成**: Model、View、Controller などのコードを自動生成
3. **データベース管理**: Migration によるスキーマのバージョン管理
4. **開発サーバー**: 組み込みの Web サーバーで即座に動作確認
5. **拡張性**: カスタムコマンドやフックによる拡張が可能

詳細な API リファレンスについては、各コマンドの実装（`src/cmd.lisp`）を参照してください。
