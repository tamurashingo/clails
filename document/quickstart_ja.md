# クイックスタート - TODO アプリを作る

このガイドでは、clails を使って簡単な TODO 管理アプリケーションを作成します。
データベースの作成、モデル・ビュー・コントローラーの実装、そして Web アプリケーションの起動までを順を追って説明します。

## 前提条件

### ローカル環境で開発する場合

- Roswell がインストールされていること
- clails がインストールされていること

インストール方法については [README.md](../README.md) を参照してください。

### Docker 環境で開発する場合（推奨）

- Docker がインストールされていること
- Docker Compose がインストールされていること

---

## 1. プロジェクトを作成する

まず、`clails new` コマンドで新しいプロジェクトを作成します。

```bash
clails new todoapp
cd todoapp
```

これにより、clails アプリケーションの基本構造が作成されます。

プロジェクト作成時に以下のファイルが生成されます：
- `Makefile` - Docker 環境での開発を簡単にするためのコマンド集
- `docker/Dockerfile.dev` - 開発用の Docker イメージ定義
- `docker/docker-compose.dev.yml` - Docker Compose 設定
- `docker/dev.env` - 開発環境の環境変数設定

---

## 2. 開発環境のセットアップ

### Docker 環境を使用する場合（推奨）

#### 2.1. Docker イメージをビルドする

```bash
make build
```

clails のブランチやタグを指定する場合は `CLAILS_BRANCH` を指定します (未指定の場合は develop が指定されます) 。

```bash
# branch
CLAILS_BRANCH=release/0.0.2 make build

# tag
CLAILS_BRANCH=v0.0.1 make build
```

イメージのビルドが完了したら、開発環境の準備は完了です。


#### 2.2. Docker コンテナを起動する

```bash
make up
```

コンテナがバックグラウンドで起動します。

#### 2.3. データベースを作成する

Docker 環境では、`make` コマンドを使ってデータベース操作を実行します。

```bash
make db.create
```

デフォルトでは SQLite3 データベースが作成されます。
MySQL や PostgreSQL を使用する場合は、プロジェクト作成時に `--database` オプションを指定してください。

**利用可能な Make コマンド:**
- `make build` - Docker イメージをビルド
- `make rebuild` - キャッシュを使わずに Docker イメージを再ビルド
- `make up` - コンテナを起動
- `make down` - コンテナを停止
- `make console` - コンテナ内でシェルを起動
- `make logs` - アプリケーションのログを表示
- `make db.create` - データベースを作成
- `make db.migrate` - マイグレーションを実行
- `make db.rollback` - マイグレーションをロールバック
- `make db.seed` - シードデータを投入

### ローカル環境を使用する場合

`clails db:create` コマンドでデータベースを作成します。

```bash
clails db:create
```

デフォルトでは SQLite3 データベースが作成されます。
MySQL や PostgreSQL を使用する場合は、プロジェクト作成時に `--database` オプションを指定してください。

---

## 3. Scaffold を生成する

`clails generate:scaffold` コマンドで、Model、View、Controller を一括生成します。

### Docker 環境を使用する場合

コンテナ内でシェルを起動して実行します：

```bash
make console
# コンテナ内で
clails generate:scaffold todo
exit
```

### ローカル環境を使用する場合

```bash
clails generate:scaffold todo
```

このコマンドにより、以下のファイルが生成されます：

- `app/models/todo.lisp` - Model ファイル
- `app/views/todo/list.html` - View ファイル
- `app/controllers/todo-controller.lisp` - Controller ファイル
- `db/migrate/YYYYMMDD-HHMMSS-todo.lisp` - Migration ファイル
- `test/models/todo.lisp` - Model のテストファイル
- `test/controllers/todo-controller.lisp` - Controller のテストファイル

---

## 4. Migration ファイルを修正する

生成された Migration ファイルを編集して、TODO テーブルの構造を定義します。

`db/migrate/YYYYMMDD-HHMMSS-todo.lisp` を開き、以下のように修正します：

```lisp
(in-package #:todoapp-db)

(defmigration "todo"
  (:up #'(lambda (connection)
           (create-table connection :table "todo"
                                    :columns '(("title" :type :string
                                                        :not-null T)
                                               ("done" :type :boolean
                                                       :default NIL)
                                               ("done-at" :type :datetime))))
   :down #'(lambda (connection)
             (drop-table connection :table "todo"))))
```

このテーブル定義では：
- `title` - TODO のタイトル（必須項目）
- `done` - 完了フラグ（デフォルト: false）
- `done_at` - 完了日時

が定義されています。

---

## 5. Migration を実行する

Migration ファイルを修正したら、`clails db:migrate` コマンドでテーブルを作成します。

### Docker 環境を使用する場合

```bash
make db.migrate
```

### ローカル環境を使用する場合

```bash
clails db:migrate
```

これにより、データベースに TODO テーブルが作成されます。

---

## 6. Model を修正する

`app/models/todo.lisp` を開き、TODO アプリに必要な機能を追加します。

```lisp
(in-package #:cl-user)
(defpackage #:todoapp/models/todo
  (:use #:cl
        #:clails/model)
  (:import-from #:clails/datetime
                #:from-universal-time
                #:format-datetime)
  (:import-from #:clails/datetime
                #:now)
  (:export #:<todo>
           #:find-all
           #:create-todo
           #:find-by-id
           #:mark-as-done
           #:format-done-at))

(in-package #:todoapp/models/todo)

(defmodel <todo> (<base-model>) (:table "todo"))

(defun find-all ()
  "Find all todo items.

   @return [list] List of todo records
   "
  (let ((q (query <todo> :as :todo)))
    (execute-query q nil)))

(defun create-todo (title)
  "Create a new todo item with the given title.

   @param title [string] Todo title
   @return [<todo>] Created todo record
   "
  (let ((todo (make-record '<todo> :title title :done nil)))
    (save todo)
    todo))

(defun find-by-id (id)
  "Find a todo item by ID.

   @param id [integer] Todo ID
   @return [<todo>] Todo record
   @return [nil] NIL if not found
   "
  (let* ((q (query <todo> :as :todo :where (:= (:todo :id) :id-param)))
         (results (execute-query q (list :id-param id))))
    (car results)))

(defun mark-as-done (todo)
  "Mark a todo item as done.

   @param todo [<todo>] Todo record to mark as done
   @return [<todo>] Updated todo record
   "
  (setf (ref todo :done) t)
  (setf (ref todo :done-at) (now))
  (save todo)
  todo)


(defmethod format-done-at ((todo <todo>))
  "Format the done-at timestamp of a todo item.

   @param todo [<todo>] Todo record
   @return [string] Formatted timestamp in MySQL format (yyyy-mm-dd hh:mm:ss)
   @return [nil] NIL if done-at is not set
   "
  (let ((done-at (ref todo :done-at)))
    (when done-at
      (format-datetime (from-universal-time done-at)
                       :format :mysql))))
```

このコードでは以下の機能を実装しています：
- `find-all` - すべての TODO を取得
- `create-todo` - 新しい TODO を作成
- `find-by-id` - ID で TODO を検索
- `mark-as-done` - TODO を完了としてマーク
- `format-done-at` - TODO の DONE-AT があれば MySQL 形式の文字列(yyyy-mm-dd hh:mm:ss)で返す
---

## 7. View を修正する

`app/views/todo/package.lisp` を開き、 View で使用するメソッドを宣言します。

```lisp
(in-package #:cl-user)
(defpackage #:todoapp/views/todo/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  (:import-from #:todoapp/models/todo
                #:format-done-at))

(in-package #:todoapp/views/todo/package)
```

`app/views/todo/list.html` を開き、TODO の一覧表示と操作ができるように修正します。

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Todo List</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        h1 { color: #333; }
        table { border-collapse: collapse; width: 100%; margin-top: 20px; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #4CAF50; color: white; }
        tr:nth-child(even) { background-color: #f2f2f2; }
        .done { text-decoration: line-through; color: #999; }
        form { margin-top: 20px; }
        input[type="text"] { padding: 5px; width: 300px; }
        button { padding: 5px 15px; background-color: #4CAF50; color: white; border: none; cursor: pointer; }
        button:hover { background-color: #45a049; }
    </style>
</head>
<body>
    <h1>Todo List</h1>
    
    <h2>Add New Todo</h2>
    <form action="/todo" method="POST">
        <input type="text" name="title" placeholder="Enter todo title" required>
        <button type="submit">Add</button>
    </form>
    
    <h2>Todo Items</h2>
    <table>
        <thead>
            <tr>
                <th>ID</th>
                <th>Title</th>
                <th>Status</th>
                <th>Done At</th>
                <th>Action</th>
            </tr>
        </thead>
        <tbody>
            <cl:loop for="todo" in="(view :todos)">
            <tr>
                <td><%= (clails/model:ref todo :id) %></td>
                <td class="<%= (if (clails/model:ref todo :done) "done" "") %>">
                    <%= (clails/model:ref todo :title) %>
                </td>
                <td><%= (if (clails/model:ref todo :done) "Done" "Pending") %></td>
                <td><%= (or (format-done-at todo) "-") %></td>
                <td>
                    <cl:unless test="(clails/model:ref todo :done)">
                    <form action="/todo" method="POST" style="display:inline;">
                        <input type="hidden" name="_method" value="PUT">
                        <input type="hidden" name="id" value="<%= (clails/model:ref todo :id) %>">
                        <button type="submit">Mark as Done</button>
                    </form>
                    </cl:unless>
                </td>
            </tr>
            </cl:loop>
        </tbody>
    </table>
    
    <p style="margin-top: 20px; color: #666;">
        Total: <%= (length (view :todos)) %> items
    </p>
</body>
</html>
```

このビューでは：
- TODO の追加フォーム
- TODO の一覧表示（テーブル形式）
- TODO を完了としてマークするボタン
- 完了した TODO の取り消し線表示

が実装されています。

**HTML フォームで PUT を実現する方法:**

HTML の `<form>` タグは `GET` と `POST` メソッドしかサポートしていません。
そのため、PUT や DELETE リクエストを送信するには、以下のように実装します：

```html
<form action="/todo/123" method="POST">
    <input type="hidden" name="_method" value="PUT">
    <button type="submit">Mark as Done</button>
</form>
```

clails は POST リクエスト内の `_method` パラメータをチェックし、値が `"PUT"` の場合は `do-put` メソッドに、`"DELETE"` の場合は `do-delete` メソッドにルーティングします。これにより、HTML フォームから PUT や DELETE リクエストを実現できます。

---

## 8. Controller を修正する

`app/controllers/todo-controller.lisp` を開き、リクエストを処理するように修正します。

```lisp
(in-package #:cl-user)
(defpackage #:todoapp/controllers/todo-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:clails/controller/base-controller
                #:param)
  (:import-from #:todoapp/models/todo
                #:find-all
                #:create-todo
                #:find-by-id
                #:mark-as-done)
  (:export #:<todo-controller>))

(in-package #:todoapp/controllers/todo-controller)

(defclass <todo-controller> (<web-controller>)
  ())

(defmethod do-get ((controller <todo-controller>))
  "Get all todo items and display them."
  (let ((todos (find-all)))
    (set-view controller "todo/list.html" (list :todos todos))))

(defmethod do-post ((controller <todo-controller>))
  "Create a new todo item."
  (let ((title (param controller "title")))
    (create-todo title)
    (set-redirect controller "/todo")))

(defmethod do-put ((controller <todo-controller>))
  "Update todo item to mark as done."
  (let* ((id-str (param controller "id"))
         (id (parse-integer id-str))
         (todo (find-by-id id)))
    (when todo
      (mark-as-done todo))
    (set-redirect controller "/todo")))
```

このコントローラーでは：
- `do-get` - TODO 一覧を表示
- `do-post` - 新しい TODO を作成
- `do-put` - TODO を完了としてマーク

が実装されています。

---

## 9. サーバーを起動する

すべての実装が完了したら、サーバーを起動します。

### Docker 環境を使用する場合

Docker 環境では、`make up` でコンテナを起動すると自動的にサーバーが起動します。

```bash
make up
```

サーバーのログを確認するには：

```bash
make logs
```

コンテナを停止するには：

```bash
make down
```

### ローカル環境を使用する場合

```bash
clails server
```

デフォルトでは `http://localhost:5000` でサーバーが起動します。

---

## 10. TODO アプリを使う

ブラウザで `http://localhost:5000/todo` にアクセスします。

以下の操作ができます：

1. **TODO の追加**: "Add New Todo" フォームにタイトルを入力して "Add" ボタンをクリック
2. **TODO の一覧表示**: 登録されているすべての TODO が表示されます
3. **TODO の完了**: "Mark as Done" ボタンをクリックすると TODO が完了状態になります

---

## （オプション）シードデータを追加する

初期データを追加したい場合は、`db/seeds.lisp` を作成します。

```lisp
(in-package #:cl-user)
(defpackage #:todoapp-db
  (:use #:cl)
  (:import-from #:clails/model
                #:save
                #:make-record)
  (:import-from #:todoapp/models/todo
                #:<todo>))

(in-package #:todoapp-db)

(defun run ()
  "Create seed data for todo table."
  (let ((todo1 (make-record '<todo> :title "Buy milk" :done nil))
        (todo2 (make-record '<todo> :title "Read a book" :done nil))
        (todo3 (make-record '<todo> :title "Write code" :done nil)))
    (save todo1)
    (save todo2)
    (save todo3)
    (format t "Created 3 todo items~%")))
```

シードデータを読み込むには：

### Docker 環境を使用する場合

```bash
make db.seed
```

### ローカル環境を使用する場合

```bash
clails db:seed
```

---

## まとめ

このクイックスタートでは、clails を使った TODO アプリケーションの作成方法を学びました。

### Docker 環境を使用した場合の主な手順：
1. プロジェクトの作成（`clails new`）
2. Docker イメージのビルド（`make build`）
3. Docker コンテナの起動（`make up`）
4. データベースの作成（`make db.create`）
5. Scaffold の生成（コンテナ内で `clails generate:scaffold`）
6. Migration ファイルの編集
7. Migration の実行（`make db.migrate`）
8. Model の実装
9. View の実装
10. Controller の実装
11. サーバーの起動（`make up` で自動起動）

### ローカル環境を使用した場合の主な手順：
1. プロジェクトの作成（`clails new`）
2. データベースの作成（`clails db:create`）
3. Scaffold の生成（`clails generate:scaffold`）
4. Migration ファイルの編集
5. Migration の実行（`clails db:migrate`）
6. Model の実装
7. View の実装
8. Controller の実装
9. サーバーの起動（`clails server`）

これらのステップを応用することで、より複雑な Web アプリケーションを作成できます。

## Docker 環境を使用する利点

- 開発環境の構築が簡単（Docker さえあれば OK）
- チーム全体で同じ環境を共有できる
- データベース（MySQL や PostgreSQL）も自動的にセットアップされる
- ホスト環境を汚さない
- `Makefile` により、よく使うコマンドを簡単に実行できる

## 次のステップ

- [Model Guide](model_ja.md) - データベース操作とクエリの詳細
- [View Guide](view_ja.md) - テンプレートエンジンの詳細
- [Controller Guide](controller_ja.md) - リクエスト処理とルーティングの詳細
- [Testing Guide](testing_ja.md) - テストの書き方
