# clails View ガイド

## 概要

clails の View は ERB（Embedded Ruby）や JSP（JavaServer Pages）のようなテンプレートエンジンです。
HTML の中に Common Lisp コードを埋め込むことができ、動的な Web ページを生成します。

## 基本概念

- View テンプレートは HTML ファイルに Common Lisp コードを埋め込んだものです
- `<%=` と `%>` で囲むと式の評価結果が出力されます
- `<%` と `%>` で囲むと Common Lisp コードを実行できます（出力なし）
- Controller から渡されたデータは `view` 関数でアクセスできます
- テンプレートはコンパイルされてキャッシュされるため、高速に動作します
- 各ディレクトリには `package.lisp` が必要です（package-inferred-system）

### フロントエンド開発について

clails のテンプレートエンジンは、シンプルなサーバーサイドレンダリングを提供します。
現代的なフロントエンド開発（React、Vue.js など）を行う場合は、clails を REST API バックエンドとして使用し、
フロントエンドは別のプロジェクトとして構築することを推奨します。

そのため、レイアウトテンプレートのような複雑な仕組みは提供していません。
各ビューは完全な HTML として記述するか、フロントエンドフレームワークと組み合わせて使用してください。

---

## 1. View テンプレートの基本

### ファイル配置

View テンプレートは `app/views/` ディレクトリに配置します。

```
app/views/
├── package.lisp        # トップレベルのパッケージ定義（自動生成）
├── index.html          # トップページ
└── users/
    ├── package.lisp    # users ディレクトリのパッケージ定義（自動生成）
    ├── index.html      # ユーザー一覧
    ├── show.html       # ユーザー詳細
    └── new.html        # ユーザー新規作成フォーム
```

### パッケージファイルについて

clails は **package-inferred-system** を採用しているため、各 View ディレクトリには `package.lisp` が**必須**です。

View テンプレートが評価されるパッケージは、そのファイルパスから自動的に決定されます。

**例**:
- `app/views/index.html` → パッケージ: `your-app/views/package`
- `app/views/todo/show.html` → パッケージ: `your-app/views/todo/package`
- `app/views/admin/users/list.html` → パッケージ: `your-app/views/admin/users/package`

`clails generate:view` コマンドで View を生成すると、`package.lisp` が自動的に作成され、
`app/application.lisp` にロード処理が追加されます。

手動で View を作成する場合は、以下の手順が必要です:

1. そのディレクトリに `package.lisp` を作成
2. `app/application.lisp` にロード処理を追加

**重要**: `package.lisp` がないディレクトリに View を配置すると、パッケージが見つからずエラーになります。

### 基本的なテンプレート

```html
<!DOCTYPE html>
<html>
<head>
  <title>Welcome</title>
</head>
<body>
  <h1>Hello, World!</h1>
  <p>This is a static content.</p>
</body>
</html>
```

---

## 2. データの埋め込み

### 式の埋め込み (`<%=` `%>`)

`<%=` と `%>` で囲んだ式の評価結果が HTML に出力されます。

```html
<!DOCTYPE html>
<html>
<head>
  <title><%= (view :title) %></title>
</head>
<body>
  <h1><%= (view :heading) %></h1>
  <p>Welcome, <%= (view :username) %>!</p>
</body>
</html>
```

Controller からのデータ受け渡し:

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (set-view controller "users/index.html"
            `(:title "ユーザー一覧"
              :heading "Users"
              :username "Taro")))
```

### `view` 関数

Controller から渡されたデータにアクセスするには `view` 関数を使います。

```html
<h1><%= (view :title) %></h1>
<p><%= (view :message) %></p>
<div>Count: <%= (view :count) %></div>
```

---

## 3. 制御構造

### スクリプトレット (`<%` `%>`)

`<%` と `%>` で囲むと Common Lisp コードを実行できます（出力はされません）。

```html
<ul>
  <% (loop for user in (view :users)
           do %>
    <li><%= (getf user :name) %></li>
  <% ) %>
</ul>
```

### 条件分岐

```html
<% (if (view :is-admin) %>
  <div class="admin-panel">
    <h2>Admin Panel</h2>
    <p>You have admin privileges.</p>
  </div>
<% ) %>

<% (if (> (view :count) 0) %>
  <p>There are <%= (view :count) %> items.</p>
<% ) %>
```

`when` や `unless` も使えます:

```html
<% (when (view :show-message) %>
  <div class="alert">
    <%= (view :message) %>
  </div>
<% ) %>

<% (unless (view :hide-footer) %>
  <footer>
    <p>&copy; 2024 Your App</p>
  </footer>
<% ) %>
```

### 繰り返し処理

#### `loop` を使う

```html
<h2>User List</h2>
<ul>
  <% (loop for user in (view :users)
           do %>
    <li>
      <strong><%= (getf user :name) %></strong>
      (<%= (getf user :email) %>)
    </li>
  <% ) %>
</ul>
```

#### `dolist` を使う

```html
<table>
  <thead>
    <tr>
      <th>ID</th>
      <th>Name</th>
      <th>Email</th>
    </tr>
  </thead>
  <tbody>
    <% (dolist (user (view :users)) %>
      <tr>
        <td><%= (getf user :id) %></td>
        <td><%= (getf user :name) %></td>
        <td><%= (getf user :email) %></td>
      </tr>
    <% ) %>
  </tbody>
</table>
```

---

## 4. Model データの表示

### Model インスタンスのレンダリング

Controller で取得した Model データを View で表示します。

Controller:

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (let ((users (execute-query
                 (query <user>
                        :as :user
                        :order-by ((:user :name)))
                 '())))
    (set-view controller "users/index.html"
              `(:users ,users))))
```

View (`app/views/users/index.html`):

```html
<!DOCTYPE html>
<html>
<head>
  <title>Users</title>
</head>
<body>
  <h1>User List</h1>
  
  <table>
    <thead>
      <tr>
        <th>ID</th>
        <th>Name</th>
        <th>Email</th>
        <th>Actions</th>
      </tr>
    </thead>
    <tbody>
      <% (dolist (user (view :users)) %>
        <tr>
          <td><%= (ref user :id) %></td>
          <td><%= (ref user :name) %></td>
          <td><%= (ref user :email) %></td>
          <td>
            <a href="/users/<%= (ref user :id) %>">View</a>
            <a href="/users/<%= (ref user :id) %>/edit">Edit</a>
          </td>
        </tr>
      <% ) %>
    </tbody>
  </table>
  
  <a href="/users/new">Create New User</a>
</body>
</html>
```

### 単一の Model インスタンス

Controller:

```common-lisp
(defmethod do-get ((controller <user-controller>))
  (let* ((id (param controller "id"))
         (user (first (execute-query
                        (query <user>
                               :as :user
                               :where (:= (:user :id) :id))
                        `(:id ,id)))))
    (set-view controller "users/show.html"
              `(:user ,user))))
```

View (`app/views/users/show.html`):

```html
<!DOCTYPE html>
<html>
<head>
  <title>User: <%= (ref (view :user) :name) %></title>
</head>
<body>
  <h1><%= (ref (view :user) :name) %></h1>
  
  <dl>
    <dt>Email</dt>
    <dd><%= (ref (view :user) :email) %></dd>
    
    <dt>Age</dt>
    <dd><%= (ref (view :user) :age) %></dd>
    
    <dt>Created At</dt>
    <dd><%= (ref (view :user) :created-at) %></dd>
  </dl>
  
  <a href="/users">Back to List</a>
  <a href="/users/<%= (ref (view :user) :id) %>/edit">Edit</a>
</body>
</html>
```

---

## 5. フォームの作成

### 基本的なフォーム

```html
<h1>Create New User</h1>

<form action="/users" method="POST">
  <div>
    <label for="name">Name:</label>
    <input type="text" id="name" name="name" required>
  </div>
  
  <div>
    <label for="email">Email:</label>
    <input type="email" id="email" name="email" required>
  </div>
  
  <div>
    <label for="age">Age:</label>
    <input type="number" id="age" name="age">
  </div>
  
  <button type="submit">Create User</button>
</form>
```

### 編集フォーム（既存データの表示）

```html
<h1>Edit User</h1>

<% (let ((user (view :user))) %>
  <form action="/users/<%= (ref user :id) %>" method="POST">
    <input type="hidden" name="_method" value="PUT">
    
    <div>
      <label for="name">Name:</label>
      <input type="text" id="name" name="name" 
             value="<%= (ref user :name) %>" required>
    </div>
    
    <div>
      <label for="email">Email:</label>
      <input type="email" id="email" name="email" 
             value="<%= (ref user :email) %>" required>
    </div>
    
    <div>
      <label for="age">Age:</label>
      <input type="number" id="age" name="age" 
             value="<%= (ref user :age) %>">
    </div>
    
    <button type="submit">Update User</button>
  </form>
<% ) %>

<a href="/users/<%= (ref (view :user) :id) %>">Cancel</a>
```

### バリデーションエラーの表示

Controller:

```common-lisp
(defmethod do-post ((controller <users-controller>))
  (let* ((name (param controller "name"))
         (email (param controller "email"))
         (user (make-record '<user> :name name :email email)))
    (if (save user)
        (set-redirect controller (format nil "/users/~A" (ref user :id)))
        (set-view controller "users/new.html"
                  `(:user ,user
                    :errors ,(get-errors user))))))

(defun get-errors (user)
  "Extract validation errors as a list"
  (loop for key being the hash-keys of (slot-value user 'clails/model/base-model::error-flag)
        using (hash-value error)
        collect (list key error)))
```

View:

```html
<h1>Create New User</h1>

<% (when (view :errors) %>
  <div class="error-messages">
    <h2>Please fix the following errors:</h2>
    <ul>
      <% (dolist (error (view :errors)) %>
        <li>
          <strong><%= (first error) %></strong>: 
          <%= (second error) %>
        </li>
      <% ) %>
    </ul>
  </div>
<% ) %>

<form action="/users" method="POST">
  <!-- フォームフィールド -->
</form>
```

---

## 6. パーシャル（部分テンプレート）

### パーシャルの作成

共通部分を別ファイルに切り出します。

`app/views/users/_user_card.html`:

```html
<% (let ((user (view :user))) %>
  <div class="user-card">
    <h3><%= (ref user :name) %></h3>
    <p>Email: <%= (ref user :email) %></p>
    <% (when (ref user :age) %>
      <p>Age: <%= (ref user :age) %></p>
    <% ) %>
    <a href="/users/<%= (ref user :id) %>">View Details</a>
  </div>
<% ) %>
```

### パーシャルの利用

パーシャルは通常のファイルとして読み込めます（将来的には専用の関数を実装予定）。

現在は、ループ内で共通のHTMLパターンを使う場合に、テンプレート内で直接記述します。

```html
<h1>Users</h1>

<div class="user-grid">
  <% (dolist (user (view :users)) %>
    <div class="user-card">
      <h3><%= (ref user :name) %></h3>
      <p>Email: <%= (ref user :email) %></p>
      <% (when (ref user :age) %>
        <p>Age: <%= (ref user :age) %></p>
      <% ) %>
      <a href="/users/<%= (ref user :id) %>">View Details</a>
    </div>
  <% ) %>
</div>
```

---

## 7. 実践的な例

### ユーザー一覧ページ

Controller:

```common-lisp
(defmethod do-get ((controller <users-controller>))
  (let ((users (execute-query
                 (query <user>
                        :as :user
                        :order-by ((:user :name)))
                 '())))
    (set-view controller "users/index.html"
              `(:title "ユーザー一覧"
                :users ,users
                :user-count ,(length users)))))
```

View:

```html
<!DOCTYPE html>
<html>
<head>
  <title><%= (view :title) %></title>
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
</head>
<body>
  <div class="container mt-4">
    <h1><%= (view :title) %></h1>
    <p>Total: <%= (view :user-count) %> users</p>
    
    <% (if (> (view :user-count) 0) %>
      <table class="table table-striped">
        <thead>
          <tr>
            <th>ID</th>
            <th>Name</th>
            <th>Email</th>
            <th>Actions</th>
          </tr>
        </thead>
        <tbody>
          <% (dolist (user (view :users)) %>
            <tr>
              <td><%= (ref user :id) %></td>
              <td><%= (ref user :name) %></td>
              <td><%= (ref user :email) %></td>
              <td>
                <a href="/users/<%= (ref user :id) %>" class="btn btn-sm btn-info">View</a>
                <a href="/users/<%= (ref user :id) %>/edit" class="btn btn-sm btn-warning">Edit</a>
              </td>
            </tr>
          <% ) %>
        </tbody>
      </table>
    <% ) (progn %>
      <div class="alert alert-info">
        <p>No users found.</p>
      </div>
    <% ) ) %>
    
    <a href="/users/new" class="btn btn-primary">Create New User</a>
  </div>
</body>
</html>
```

### ダッシュボードページ

Controller:

```common-lisp
(defmethod do-get ((controller <dashboard-controller>))
  (let ((user-count (count-all-users))
        (recent-users (get-recent-users 5))
        (stats (get-statistics)))
    (set-view controller "dashboard/index.html"
              `(:title "Dashboard"
                :user-count ,user-count
                :recent-users ,recent-users
                :stats ,stats))))
```

View:

```html
<!DOCTYPE html>
<html>
<head>
  <title><%= (view :title) %></title>
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">
</head>
<body>
  <div class="container mt-4">
    <h1><%= (view :title) %></h1>
    
    <!-- Statistics Cards -->
    <div class="row mt-4">
      <div class="col-md-4">
        <div class="card text-white bg-primary">
          <div class="card-body">
            <h5 class="card-title">Total Users</h5>
            <p class="card-text display-4"><%= (view :user-count) %></p>
          </div>
        </div>
      </div>
      
      <div class="col-md-4">
        <div class="card text-white bg-success">
          <div class="card-body">
            <h5 class="card-title">Active Sessions</h5>
            <p class="card-text display-4"><%= (getf (view :stats) :active-sessions) %></p>
          </div>
        </div>
      </div>
      
      <div class="col-md-4">
        <div class="card text-white bg-info">
          <div class="card-body">
            <h5 class="card-title">Today's Signups</h5>
            <p class="card-text display-4"><%= (getf (view :stats) :today-signups) %></p>
          </div>
        </div>
      </div>
    </div>
    
    <!-- Recent Users -->
    <div class="card mt-4">
      <div class="card-header">
        <h5>Recent Users</h5>
      </div>
      <div class="card-body">
        <table class="table">
          <thead>
            <tr>
              <th>Name</th>
              <th>Email</th>
              <th>Joined</th>
            </tr>
          </thead>
          <tbody>
            <% (dolist (user (view :recent-users)) %>
              <tr>
                <td><%= (ref user :name) %></td>
                <td><%= (ref user :email) %></td>
                <td><%= (format-date (ref user :created-at)) %></td>
              </tr>
            <% ) %>
          </tbody>
        </table>
      </div>
    </div>
  </div>
</body>
</html>
```

---

## 8. View パッケージ

### パッケージの重要性

clails は **package-inferred-system** を採用しているため、各 View ディレクトリには `package.lisp` が**必須**です。

View テンプレートは、そのファイルパスに基づいて決定されたパッケージ内で評価されます。
パッケージが存在しない場合、テンプレートの評価時にエラーが発生します。

### パッケージの自動解決

View テンプレートのファイルパスから、評価されるパッケージが自動的に決定されます。

**パスとパッケージの対応**:
- `app/views/index.html` → `:your-app/views/package`
- `app/views/todo/show.html` → `:your-app/views/todo/package`
- `app/views/admin/users/list.html` → `:your-app/views/admin/users/package`

このため、対応する `package.lisp` が必要です:
- `app/views/package.lisp` - トップレベルのパッケージ定義
- `app/views/todo/package.lisp` - todo ディレクトリのパッケージ定義
- `app/views/admin/users/package.lisp` - admin/users ディレクトリのパッケージ定義

### 自動生成される package.lisp

`clails generate:view todo` を実行すると、以下のようなファイルが生成されます。

`app/views/todo/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/todo/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view))

(in-package #:your-app/views/todo/package)
```

同時に、`app/application.lisp` に以下のようなロード処理が追加されます:

```common-lisp
(load (merge-pathnames "app/views/todo/package.lisp" *project-dir*))
```

### 手動でパッケージを作成する

手動で View を作成する場合は、**必ず**以下の手順でパッケージを設定してください。

#### 1. package.lisp を作成

例えば、`app/views/todo/` ディレクトリに View を作成する場合:

`app/views/todo/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/todo/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  (:import-from #:your-app/model
                #:ref))

(in-package #:your-app/views/todo/package)
```

#### 2. app/application.lisp にロード処理を追加

`app/application.lisp` に以下を追加:

```common-lisp
;; View パッケージのロード
(load (merge-pathnames "app/views/todo/package.lisp" *project-dir*))
```

**重要**: この手順を省略すると、`app/views/todo/` 配下の View テンプレートを評価する際に、
パッケージが見つからずエラーが発生します。

### パッケージによる関数の簡潔化

`package.lisp` で関数をインポートすることで、テンプレート内で短く記述できます。

#### 完全修飾名 vs インポート後

```html
<!-- インポートなし: 完全修飾名が必要 -->
<h1><%= (clails/view/view-helper:view :title) %></h1>
<p><%= (your-app/model:ref (clails/view/view-helper:view :user) :name) %></p>

<!-- インポートあり: 関数名だけで呼び出せる -->
<h1><%= (view :title) %></h1>
<p><%= (ref (view :user) :name) %></p>
```

#### 基本的なインポート

`app/views/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view))

(in-package #:your-app/views/package)
```

**ポイント**:
- `:use #:cl` - Common Lisp の標準関数を使用可能にする
- `:import-from` - 必要な関数を個別にインポート
- `view` - Controller から渡されたデータにアクセスする関数
- `*view-context*` - View コンテキストを保持する特殊変数（高度な使用）

#### Model 関数のインポート

Model の `ref` 関数などもインポートすると便利です。

`app/views/todo/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/todo/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  (:import-from #:your-app/model
                #:ref
                #:<todo>))

(in-package #:your-app/views/todo/package)
```

これにより、テンプレート内で `ref` を直接使用できます:

```html
<!-- app/views/todo/show.html -->
<h1><%= (ref (view :todo) :title) %></h1>
<p><%= (ref (view :todo) :description) %></p>
```

#### Helper 関数のインポート

Controller に定義したヘルパー関数もインポートできます。

`app/views/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  (:import-from #:your-app/controllers/application-controller
                #:format-date
                #:format-currency
                #:current-user)
  (:import-from #:your-app/model
                #:ref))

(in-package #:your-app/views/package)
```

テンプレート内でヘルパー関数を使用:

```html
<p>Created: <%= (format-date (ref user :created-at)) %></p>
<p>Price: <%= (format-currency (ref product :price)) %></p>

<% (when (current-user) %>
  <p>Welcome, <%= (ref (current-user) :name) %>!</p>
<% ) %>
```

### パッケージを使う利点

#### 1. コードの簡潔化

パッケージで関数をインポートすることで、テンプレート内のコードが短く読みやすくなります。

```html
<!-- インポートなし: 完全修飾名が必要 -->
<%= (clails/view/view-helper:view :title) %>
<%= (your-app/model:ref user :name) %>

<!-- インポートあり: 関数名だけで呼び出せる -->
<%= (view :title) %>
<%= (ref user :name) %>
```

#### 2. 名前空間の管理

各 View ディレクトリごとに独立したパッケージを持つことで、名前の衝突を避けられます。

```common-lisp
;; app/views/admin/package.lisp
(defpackage #:your-app/views/admin/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper #:view)
  (:import-from #:your-app/controllers/admin-controller
                #:admin-only-function))

;; app/views/public/package.lisp
(defpackage #:your-app/views/public/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper #:view)
  (:import-from #:your-app/controllers/public-controller
                #:public-function))
```

#### 3. 型やクラスのインポート

Model クラスをインポートすることで、型チェックなどに利用できます。

```common-lisp
(defpackage #:your-app/views/users/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper #:view)
  (:import-from #:your-app/model
                #:ref
                #:<user>
                #:<post>))
```

### 実践的な例

#### フルスペックのパッケージ定義

`app/views/users/package.lisp`:

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app/views/users/package
  (:use #:cl)
  ;; View ヘルパー
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  ;; Model 関数とクラス
  (:import-from #:your-app/model
                #:ref
                #:<user>
                #:<post>
                #:has-error-p
                #:ref-error)
  ;; Controller ヘルパー
  (:import-from #:your-app/controllers/application-controller
                #:format-datetime
                #:truncate-text
                #:gravatar-url)
  ;; ユーティリティ関数
  (:import-from #:alexandria
                #:when-let))

(in-package #:your-app/views/users/package)
```

#### テンプレートでの使用例

`app/views/users/show.html`:

```html
<!DOCTYPE html>
<html>
<head>
  <title>User: <%= (ref (view :user) :name) %></title>
</head>
<body>
  <% (let ((user (view :user))) %>
    <div class="user-profile">
      <img src="<%= (gravatar-url (ref user :email)) %>" alt="Avatar">
      <h1><%= (ref user :name) %></h1>
      <p class="email"><%= (ref user :email) %></p>
      
      <% (when-let (bio (ref user :bio)) %>
        <div class="bio">
          <%= (truncate-text bio 200) %>
        </div>
      <% ) %>
      
      <p class="joined">
        Joined: <%= (format-datetime (ref user :created-at)) %>
      </p>
      
      <% (when (has-error-p user) %>
        <div class="errors">
          <p><%= (ref-error user :name) %></p>
        </div>
      <% ) %>
    </div>
  <% ) %>
</body>
</html>
```

### パッケージのデバッグ

テンプレート内で現在のパッケージを確認できます。

```html
<!-- 現在のパッケージを表示 -->
<p>Current package: <%= (format nil "~A" *package*) %></p>

<!-- 出力例: Current package: #<PACKAGE "YOUR-APP/VIEWS/USERS/PACKAGE"> -->
```

---

## 9. ベストプラクティス

### View の責務

View は以下の責務のみを持つべきです。

1. **データの表示**: Controller から渡されたデータを表示
2. **表示ロジック**: 条件分岐や繰り返しなど、表示に必要な最小限のロジック
3. **フォームの生成**: ユーザー入力を受け取るフォームの生成

### ロジックの分離

複雑なロジックは View に書かず、Controller や Model に配置します。

```html
<!-- 悪い例: View に複雑なロジック -->
<% (let ((filtered-users
          (remove-if-not 
           #'(lambda (u) 
               (and (> (ref u :age) 18)
                    (string= (ref u :status) "active")))
           (view :users)))) %>
  <!-- ... -->
<% ) %>

<!-- 良い例: Controller でフィルタリング済みのデータを渡す -->
<% (dolist (user (view :active-adult-users)) %>
  <!-- ... -->
<% ) %>
```

### データの準備

View で必要なデータは Controller で準備します。

```common-lisp
;; 良い例
(defmethod do-get ((controller <users-controller>))
  (let* ((users (get-all-users))
         (active-users (remove-if-not #'user-active-p users))
         (user-count (length users)))
    (set-view controller "users/index.html"
              `(:users ,active-users
                :total-count ,user-count
                :active-count ,(length active-users)))))
```

### HTMLエスケープ

ユーザー入力を表示する場合は、XSS対策のため適切にエスケープします。

```html
<!-- 現在は自動エスケープ機能は未実装 -->
<!-- ユーザー入力を表示する場合は注意が必要 -->
<p>Name: <%= (view :user-input) %></p>
```

### コメント

テンプレート内のコメントは HTML コメントまたは Common Lisp コメントを使用します。

```html
<!-- HTML コメント: ブラウザに送信される -->

<% #|
Common Lisp ブロックコメント: 
出力には含まれない
|# %>

<% ; Common Lisp 行コメント %>
```

---

## 10. パフォーマンス

### テンプレートのキャッシュ

テンプレートはコンパイルされてキャッシュされるため、2回目以降のレンダリングは高速です。

### 開発時のキャッシュクリア

開発中にテンプレートを変更した場合は、キャッシュをクリアする必要があります（自動リロード機能は今後実装予定）。

```common-lisp
;; キャッシュのクリア方法（実装によって異なる）
(clails/view/cache:clear-cache)
```

---

## まとめ

clails の View は以下の特徴を持ちます。

1. **シンプルな構文**: `<%=` と `<%` による直感的なテンプレート記法
2. **Common Lisp の活用**: テンプレート内で Common Lisp の全機能を使用可能
3. **Controller との統合**: `view` 関数による簡単なデータアクセス
4. **高速な実行**: テンプレートのコンパイルとキャッシュによる高速化
5. **柔軟な構造**: パッケージシステムによる名前空間の管理

詳細な API リファレンスについては、各関数の docstring を参照してください。
