# clails Model ガイド

## 概要

clails の Model は Ruby on Rails の ActiveRecord を参考にしたデータベースアクセスレイヤーです。
ただし、Rails とは異なり、すべてのカラム情報をハッシュテーブルで管理し、カラムへのアクセスには専用の関数を使用します。

## 基本概念

- Model はデータベーステーブルと対応します
- カラム情報は直接クラス定義に記述せず、データベースから自動的に取得されます
- カラムへのアクセスには `ref` 関数、値の設定には `(setf ref)` を使用します
- 変更されたカラムのみが更新されます（dirty flag による追跡）

---

## 1. Migration ファイルの書き方

Migration ファイルはデータベースのスキーマ変更を管理するファイルです。
`db/migrate/` ディレクトリ配下に配置します。

### ファイル命名規則

```
YYYYmmdd-HHMMSS-description.lisp
```

例: `20240101-120000-create-users-table.lisp`

### テーブル作成

```common-lisp
(in-package #:your-app/db/migrate)

(defmigration "20240101-120000-create-users-table"
  (:up #'(lambda (conn)
           (create-table conn :table "users"
                              :columns '(("name" :type :string
                                                 :not-null T)
                                         ("email" :type :string
                                                  :not-null T)
                                         ("age" :type :integer
                                                :not-null NIL)
                                         ("is-active" :type :boolean
                                                      :default-value T))))
   :down #'(lambda (conn)
             (drop-table conn :table "users"))))
```

### カラムの追加

```common-lisp
(defmigration "20240102-130000-add-phone-to-users"
  (:up #'(lambda (conn)
           (add-column conn :table "users"
                            :columns '(("phone" :type :string
                                                :not-null NIL))))
   :down #'(lambda (conn)
             (drop-column conn :table "users"
                               :column "phone"))))
```

### インデックスの追加

```common-lisp
(defmigration "20240103-140000-add-index-to-users"
  (:up #'(lambda (conn)
           (add-index conn :table "users"
                           :index "idx-users-email"
                           :columns '("email"))))
   :down #'(lambda (conn)
             (drop-index conn :table "users"
                              :index "idx-users-email"))))
```

### カラムタイプ一覧

- `:string` - 文字列（VARCHAR）
- `:text` - テキスト（TEXT）
- `:integer` - 整数
- `:float` - 浮動小数点数
- `:decimal` - 固定小数点数
- `:datetime` - 日時
- `:date` - 日付
- `:time` - 時刻
- `:boolean` - 真偽値

### カラムオプション

- `:not-null` - NULL を許可しない場合は `T`、許可する場合は `NIL`
- `:default-value` - デフォルト値を指定

### Migration の実行

```common-lisp
;; データベース作成
(clails/model/migration:db-create)

;; Migration 実行
(clails/model/migration:db-migrate)

;; Migration ステータス確認
(clails/model/migration:db-status)
```

---

## 2. Model の定義

### 基本的な Model 定義

```common-lisp
(in-package #:your-app/model)

(defmodel <user> (<base-model>)
  (:table "users"))
```

これだけで、`users` テーブルに対応する Model が定義されます。
カラム情報はデータベースから自動的に取得されます。

### テーブル情報の初期化

アプリケーション起動時に一度だけ実行します。

```common-lisp
(clails/model/base-model:initialize-table-information)
```

---

## 3. 親子関係のある Model の定義

### 関連の種類

- `:belongs-to` - 親への参照（外部キーを持つ側）
- `:has-many` - 子への参照（外部キーを持たない側）

### サンプル: 会社と部署の関係

```common-lisp
;; 親 Model
(defmodel <company> (<base-model>)
  (:table "company"
   :relations ((:has-many "your-app/model::<department>"
                 :as :departments
                 :foreign-key :company-id))))

;; 子 Model
(defmodel <department> (<base-model>)
  (:table "department"
   :relations ((:belongs-to "your-app/model::<company>"
                 :column :company
                 :key :company-id)
               (:has-many "your-app/model::<employee>"
                 :as :employees
                 :foreign-key :department-id))))

;; 孫 Model
(defmodel <employee> (<base-model>)
  (:table "employee"
   :relations ((:belongs-to "your-app/model::<department>"
                 :column :department
                 :key :department-id))))
```

### 関連のパラメータ

#### `:has-many` の場合

- `:as` - 関連にアクセスするためのエイリアス（キーワード）
- `:foreign-key` - 子テーブルが持つ外部キー（キーワード）

#### `:belongs-to` の場合

- `:column` - 親にアクセスするためのエイリアス（キーワード）
- `:key` - 自テーブルが持つ外部キー（キーワード）

---

## 4. データの作成 (make-record)

新しいレコードのインスタンスを作成します。

```common-lisp
;; 新規インスタンス作成
(defvar *user* (make-record '<user>
                            :name "Taro Yamada"
                            :email "taro@example.com"
                            :age 30
                            :is-active T))

;; カラムの値を取得
(ref *user* :name)      ; => "Taro Yamada"
(ref *user* :email)     ; => "taro@example.com"
(ref *user* :age)       ; => 30
(ref *user* :is-active) ; => T

;; カラムの値を設定
(setf (ref *user* :age) 31)
```

---

## 5. クエリの書き方

### 基本的なクエリ

```common-lisp
;; 全件取得
(defvar *users* (execute-query
                  (query <user>
                         :as :user)
                  '()))

;; WHERE 句を使用
(defvar *active-users* (execute-query
                         (query <user>
                                :as :user
                                :where (:= (:user :is-active) T))
                         '()))

;; 並び替え
(defvar *sorted-users* (execute-query
                         (query <user>
                                :as :user
                                :order-by ((:user :created-at :desc)))
                         '()))

;; LIMIT と OFFSET
(defvar *page-users* (execute-query
                       (query <user>
                              :as :user
                              :limit 10
                              :offset 20)
                       '()))
```

### WHERE 句の演算子

- `:=` - 等しい
- `:>` - より大きい
- `:<` - より小さい
- `:>=` - 以上
- `:<=` - 以下
- `:!=` - 等しくない
- `:null` - NULL である
- `:not-null` - NULL でない
- `:in` - IN 句
- `:not-in` - NOT IN 句
- `:and` - AND 条件
- `:or` - OR 条件
- `:like` - LIKE 句

### WHERE 句のサンプル

```common-lisp
;; 比較演算子
(query <user>
       :as :user
       :where (:> (:user :age) 20))

;; NULL チェック
(query <user>
       :as :user
       :where (:null (:user :phone)))

;; IN 句
(query <user>
       :as :user
       :where (:in (:user :id) (1 2 3)))

;; IN 句（パラメータ使用）
(execute-query
  (query <user>
         :as :user
         :where (:in (:user :id) :ids))
  '(:ids (1 2 3)))

;; AND 条件
(query <user>
       :as :user
       :where (:and (:= (:user :is-active) T)
                    (:>= (:user :age) 18)))

;; OR 条件
(query <user>
       :as :user
       :where (:or (:= (:user :age) 20)
                   (:= (:user :age) 30)))

;; LIKE 句
(query <user>
       :as :user
       :where (:like (:user :email) "%@example.com"))
```

### ORDER BY 句

```common-lisp
;; 昇順
(query <user>
       :as :user
       :order-by ((:user :name)))

;; 降順
(query <user>
       :as :user
       :order-by ((:user :created-at :desc)))

;; 複数カラム
(query <user>
       :as :user
       :order-by ((:user :age :desc)
                  (:user :name)))
```

---

## 6. JOIN を含むクエリの書き方

### INNER JOIN

```common-lisp
;; 部署と会社を JOIN
(defvar *departments*
  (execute-query
    (query <department>
           :as :dept
           :columns ((dept :id :name)
                     (company :name))
           :joins ((:inner-join :company)))
    '()))

;; 結果の取得
(dolist (dept *departments*)
  (format t "Department: ~A, Company: ~A~%"
          (ref dept :name)
          (ref (ref dept :company) :name)))

;; ref-in を使った簡潔な書き方
(dolist (dept *departments*)
  (format t "Department: ~A, Company: ~A~%"
          (ref dept :name)
          (ref-in dept :company :name)))
```

### LEFT JOIN

```common-lisp
(defvar *departments*
  (execute-query
    (query <department>
           :as :dept
           :joins ((:left-join :company)))
    '()))
```

### 多段 JOIN

```common-lisp
;; 従業員 → 部署 → 会社
(defvar *employees*
  (execute-query
    (query <employee>
           :as :emp
           :columns ((emp :id :name :employee-number)
                     (dept :name)
                     (company :name))
           :joins ((:inner-join :department)
                   (:inner-join :company :through :department)))
    '()))

;; 結果の取得
(dolist (emp *employees*)
  (format t "Employee: ~A, Department: ~A, Company: ~A~%"
          (ref emp :name)
          (ref-in emp :department :name)
          (ref-in emp :company :name)))
```

### JOIN と WHERE 句の組み合わせ

```common-lisp
(defvar *filtered-employees*
  (execute-query
    (query <employee>
           :as :emp
           :joins ((:inner-join :department))
           :where (:= (:dept :name) "Sales"))
    '()))
```

### ref と ref-in 関数

JOIN したデータにアクセスするには `ref` 関数を使います。

```common-lisp
;; 基本的な使い方
(ref dept :name)              ; 部署名
(ref (ref dept :company) :name) ; 会社名（JOIN したデータ）

;; JOIN したデータの取得例
(dolist (dept *departments*)
  (format t "Department: ~A, Company: ~A~%"
          (ref dept :name)
          (ref (ref dept :company) :name)))
```

`ref-in` は複数回の `ref` 呼び出しを簡略化するマクロです。リレーションやリストのインデックスを組み合わせてネストしたデータに簡潔にアクセスできます。

```common-lisp
;; ref-in の使い方
(ref-in employee :department :name)        ; (ref (ref employee :department) :name) と同じ
(ref-in employee :company :name)           ; (ref (ref employee :company) :name) と同じ

;; リストとリレーションの組み合わせ
(ref-in blog :comments 0 :comment-account :username)
;; => (ref (ref (nth 0 (ref blog :comments)) :comment-account) :username) に展開される

;; JOIN と ref-in の組み合わせ例
(dolist (emp *employees*)
  (format t "Employee: ~A, Department: ~A, Company: ~A~%"
          (ref emp :name)
          (ref-in emp :department :name)
          (ref-in emp :company :name)))
```

---

## 7. データの保存

### INSERT と UPDATE

`save` 関数は自動的に INSERT または UPDATE を判断します。

```common-lisp
;; INSERT（ID が nil の場合）
(defvar *new-user* (make-record '<user>
                                :name "Hanako Suzuki"
                                :email "hanako@example.com"
                                :age 25))
(save *new-user*)
;; 保存後、ID が自動的に設定されます
(ref *new-user* :id) ; => 1

;; UPDATE（ID が存在し、dirty flag が立っている場合）
(setf (ref *new-user* :age) 26)
(save *new-user*)

;; dirty flag がない場合は何もしない
(save *new-user*) ; => *new-user* (変更がないため UPDATE は実行されない)
```

### デフォルト値の扱い

明示的に値を設定しなかったカラムには、データベースで定義されたデフォルト値が適用されます。

```common-lisp
;; Migration でデフォルト値を定義
;; ("status" :type :string :default-value "active")

;; status を設定せずに保存
(defvar *product* (make-record '<product> :name "Sample"))
(save *product*)

;; データベースのデフォルト値が適用される
(ref *product* :status) ; => "active"
```

### Validation

`validate` メソッドをオーバーライドしてバリデーションを実装します。

```common-lisp
(defmethod validate ((inst <user>))
  (when (or (null (ref inst :name))
            (string= (ref inst :name) ""))
    (setf (ref-error inst :name)
          "Name is required"))
  
  (when (or (null (ref inst :email))
            (string= (ref inst :email) ""))
    (setf (ref-error inst :email)
          "Email is required")))

;; バリデーションエラーがある場合、save は NIL を返す
(defvar *invalid-user* (make-record '<user> :name "" :email ""))
(save *invalid-user*) ; => NIL

;; エラーの確認
(has-error-p *invalid-user*) ; => T
(ref-error *invalid-user* :name) ; => "Name is required"
(ref-error *invalid-user* :email) ; => "Email is required"
```

### 楽観的ロック

楽観的ロックを使用するには、Migration でバージョン管理用のカラムを作成し、`defmodel` で `:version` オプションを指定します。

```common-lisp
;; Migration: バージョン管理用のカラムを作成
(defmigration "20251001-000000-create-posts-table"
  (:up #'(lambda (conn)
           (create-table conn :table "posts"
                              :columns '(("title" :type :string :not-null T)
                                         ("content" :type :text)
                                         ("lock-version" :type :integer
                                                         :not-null T
                                                         :default-value 0))))
   :down #'(lambda (conn)
             (drop-table conn :table "posts"))))

;; Model: :version オプションでバージョン管理カラムを指定
(defmodel <post> (<base-model>)
  (:table "posts"
   :version :lock-version))
```

楽観的ロックが有効な場合、更新時にバージョンが一致しないと `optimistic-lock-error` が発生します。

```common-lisp
;; 同じレコードを2つのインスタンスで取得
(defvar *post1* (first (execute-query
                         (query <post>
                                :as :post
                                :where (:= (:post :id) 1))
                         '())))
(defvar *post2* (first (execute-query
                         (query <post>
                                :as :post
                                :where (:= (:post :id) 1))
                         '())))

;; post1 を更新（成功）
(setf (ref *post1* :title) "Updated by user 1")
(save *post1*)
(ref *post1* :lock-version) ; => 1

;; post2 を更新（失敗: バージョンが古い）
(setf (ref *post2* :title) "Updated by user 2")
(handler-case
    (save *post2*)
  (clails/condition:optimistic-lock-error ()
    (format t "Record was modified by another process~%")))
```

注意: `lock-version` という名前のカラムがあっても、`defmodel` で `:version` オプションを指定しなければ楽観的ロックは有効になりません。

---

## 8. トランザクション管理

clails では、データベーストランザクションを簡単に扱うための `with-transaction` マクロを提供しています。

### 基本的な使い方

```common-lisp
(clails/model/transaction:with-transaction
  ;; トランザクション内で実行される処理
  (let ((user (make-record '<user> :name "Taro" :email "taro@example.com")))
    (save user))
  (let ((profile (make-record '<profile> :user-id (ref user :id) :bio "Hello")))
    (save profile)))
```

### トランザクションの動作

- **自動コミット**: ブロック内の処理がすべて正常に完了すると、自動的にコミットされます
- **自動ロールバック**: ブロック内でエラーが発生すると、自動的にロールバックされます
- **コネクション管理**: コネクションの取得と解放は自動的に行われます

```common-lisp
;; 正常終了の場合: 自動コミット
(with-transaction
  (let ((user (make-record '<user> :name "Taro")))
    (save user)))
;; => ここでコミットされる

;; エラー発生の場合: 自動ロールバック
(handler-case
    (with-transaction
      (let ((user (make-record '<user> :name "Taro")))
        (save user))
      (error "Something went wrong"))
  (error (e)
    ;; トランザクションは自動的にロールバックされる
    (format t "Transaction rolled back: ~A~%" e)))
```

### ネストしたトランザクション（セーブポイント）

`with-transaction` はネストして使用でき、ネストされたトランザクションは自動的にセーブポイントとして実装されます。

```common-lisp
(with-transaction
  ;; 外側のトランザクション
  (let ((company (make-record '<company> :name "ACME Corp")))
    (save company))
  
  ;; 内側のトランザクション（セーブポイント）
  (handler-case
      (with-transaction
        (let ((department (make-record '<department> 
                                      :name "Sales"
                                      :company-id (ref company :id))))
          (save department))
        (error "Department creation failed"))
    (error (e)
      ;; 内側のトランザクションのみロールバックされる
      ;; company は保存される
      (format t "Department transaction rolled back~%"))))
```

### ネストしたトランザクションの挙動

#### 両方コミット

```common-lisp
(with-transaction
  ;; 外側のデータを保存
  (save outer-record)
  
  (with-transaction
    ;; 内側のデータを保存
    (save inner-record)))
;; => 両方のデータが保存される
```

#### 内側のみロールバック

```common-lisp
(with-transaction
  (save outer-record)
  
  (handler-case
      (with-transaction
        (save inner-record)
        (error "Inner error"))
    (error (e) nil)))
;; => outer-record のみ保存される
;; => inner-record はロールバックされる
```

#### 外側ごとロールバック

```common-lisp
(handler-case
    (with-transaction
      (save outer-record)
      
      (with-transaction
        (save inner-record))
      
      (error "Outer error"))
  (error (e) nil))
;; => すべてロールバックされる
```

### 複数のモデルを扱う場合

```common-lisp
(defun create-user-with-profile (name email bio)
  "ユーザーとプロフィールをトランザクション内で作成"
  (with-transaction
    (let* ((user (make-record '<user> :name name :email email))
           (saved-user (save user)))
      (when saved-user
        (let ((profile (make-record '<profile> 
                                   :user-id (ref saved-user :id)
                                   :bio bio)))
          (save profile))))))

;; 使用例
(create-user-with-profile "Taro Yamada" "taro@example.com" "Hello, World!")
;; => ユーザーとプロフィールの両方が作成される
;; => どちらかが失敗すると、両方ロールバックされる
```

### トランザクション内でのクエリ実行

トランザクション内では、通常通りクエリを実行できます。

```common-lisp
(with-transaction
  ;; データの作成
  (let ((user (make-record '<user> :name "Taro")))
    (save user))
  
  ;; 作成したデータの検索
  (let ((users (execute-query
                 (query <user>
                        :as :user
                        :where (:= (:user :name) :name))
                 '(:name "Taro"))))
    (format t "Found ~A users~%" (length users))))
```

### 明示的なロールバック

トランザクションを明示的にロールバックしたい場合は、エラーを発生させます。

```common-lisp
(handler-case
    (with-transaction
      (save user)
      
      ;; 何か条件によってロールバックしたい場合
      (when (some-condition-p)
        (error "Explicit rollback"))
      
      (save profile))
  (error (e)
    (format t "Transaction aborted: ~A~%" e)))
```

### 注意事項

1. **ネストの深さ**: 過度なネストは避け、必要な場合のみ使用してください
2. **長時間のトランザクション**: トランザクションは短時間で完了するように設計してください
3. **デッドロック**: 複数のトランザクションが同じリソースにアクセスする場合、デッドロックに注意してください
4. **コネクション管理**: `with-transaction` はスレッドローカルなコネクションを使用します。同じスレッド内では同じコネクションが再利用されます

---

## 9. データの削除

### 単一レコードの削除

```common-lisp
(defvar *user* (first (execute-query
                        (query <user>
                               :as :user
                               :where (:= (:user :id) 1))
                        '())))

;; 削除
(destroy *user*)

;; 削除後は frozen-p が T になり、変更できなくなる
(frozen-p *user*) ; => T
(save *user*)     ; => NIL (frozen なので保存できない)
```

### 複数レコードの削除

```common-lisp
(defvar *users* (execute-query
                  (query <user>
                         :as :user
                         :where (:= (:user :is-active) NIL))
                  '()))

;; 一括削除
(destroy *users*)
```

### カスケード削除

`:cascade T` を指定すると、`:has-many` で定義された関連レコードも削除されます。

```common-lisp
;; 会社を削除すると、その会社に属する部署もすべて削除される
(destroy *company* :cascade T)

;; 部署を削除すると、その部署に属する従業員もすべて削除される
(destroy *department* :cascade T)
```

### トランザクション内での削除

削除処理もトランザクション内で実行できます。

```common-lisp
(with-transaction
  ;; 複数のレコードを削除
  (destroy user1)
  (destroy user2)
  ;; どちらかが失敗すると、両方ロールバックされる
  )
```

---

## 10. その他の便利な機能

### dirty flag の確認

```common-lisp
(defvar *user* (make-record '<user> :name "Test"))
(has-dirty-p *user*) ; => T (新規作成時は dirty)

(save *user*)
(has-dirty-p *user*) ; => NIL (保存後は dirty flag がクリアされる)

(setf (ref *user* :name) "New Name")
(has-dirty-p *user*) ; => T (変更があったので dirty)
```

### エラーのクリア

```common-lisp
(clear-error *user*)
(has-error-p *user*) ; => NIL
```

### JSON への変換

Model インスタンスは自動的に JSON に変換できます。

```common-lisp
(jonathan:to-json *user*)
;; => "{\"ID\":1,\"NAME\":\"Taro Yamada\",\"EMAIL\":\"taro@example.com\",...}"
```

---

## 11. よくある使用パターン

### ページネーション

```common-lisp
(defun get-users-page (page per-page)
  (let ((offset (* (1- page) per-page)))
    (execute-query
      (query <user>
             :as :user
             :order-by ((:user :created-at :desc))
             :limit per-page
             :offset offset)
      '())))

;; 使用例
(get-users-page 1 20)  ; 1ページ目（1-20件）
(get-users-page 2 20)  ; 2ページ目（21-40件）
```

### 検索

```common-lisp
(defun search-users-by-name (keyword)
  (execute-query
    (query <user>
           :as :user
           :where (:like (:user :name) :keyword)
           :order-by ((:user :name)))
    (list :keyword (format nil "%~A%" keyword))))
```

### バッチ処理とトランザクション

大量のデータを処理する場合は、トランザクションを適切に使用します。

```common-lisp
(defun import-users (user-data-list)
  "ユーザーデータを一括インポート"
  (with-transaction
    (loop for user-data in user-data-list
          do (let ((user (make-record '<user>
                                     :name (getf user-data :name)
                                     :email (getf user-data :email))))
               (unless (save user)
                 (error "Failed to save user: ~A" (getf user-data :name)))))))

;; 使用例
(import-users '((:name "User1" :email "user1@example.com")
                (:name "User2" :email "user2@example.com")
                (:name "User3" :email "user3@example.com")))
;; => すべて成功するか、すべて失敗する
```

### 条件付き保存

```common-lisp
(defun update-user-if-not-modified (user-id new-name expected-version)
  "バージョンが一致する場合のみユーザーを更新"
  (with-transaction
    (let ((user (first (execute-query
                        (query <user>
                               :as :user
                               :where (:= (:user :id) :id))
                        (list :id user-id)))))
      (when user
        (if (= (ref user :lock-version) expected-version)
            (progn
              (setf (ref user :name) new-name)
              (save user)
              T)
            (progn
              (format t "User was modified by another process~%")
              NIL))))))
```

---

## まとめ

clails の Model は以下の特徴を持ちます。

1. **明示的な設計**: カラム情報は自動取得され、アクセスは専用の関数を使用
2. **効率的な更新**: dirty flag により、変更されたカラムのみを更新
3. **柔軟なクエリ**: DSL によるクエリ構築で、JOIN や複雑な条件も記述可能
4. **関連の管理**: `:belongs-to` と `:has-many` による親子関係の定義
5. **安全性**: バリデーション、楽観的ロック、トランザクションのサポート
6. **トランザクション管理**: `with-transaction` による簡単なトランザクション制御とネストしたトランザクション（セーブポイント）のサポート

詳細な API リファレンスについては、各関数の docstring を参照してください。
