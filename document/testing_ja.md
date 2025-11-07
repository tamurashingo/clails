# clails テストガイド

## 概要

このガイドでは、clailsで構築されたアプリケーションのテストの書き方と実行方法を説明します。
clailsは、タグやパッケージフィルタリングなどの追加機能を備えた、**Rove**ベースのテストフレームワークを提供しています。

## 基本概念

- テストはclails拡張機能を備えた**Rove**テストフレームワークを使用
- テストは`deftest-suite`マクロを使用して定義し、タグ付けをサポート
- テストはタグまたはパッケージでフィルタリングして実行可能
- アプリケーション内でテストを実行するには`clails test`コマンドを使用
- テストファイルは`test/`ディレクトリに整理され、アプリケーション構造をミラーリング

---

## 1. テストプロジェクトの構造

新しいclailsプロジェクトを作成すると、テスト構造が自動的に生成されます：

```
your-app/
├── app/
│   ├── controllers/
│   ├── models/
│   └── views/
├── test/
│   ├── controllers/      # コントローラーテスト
│   ├── models/           # モデルテスト
│   ├── views/            # ビューテスト
│   ├── sample.lisp       # サンプルテストファイル
│   └── test-loader.lisp  # テストローダー設定
├── your-app.asd          # アプリケーションシステム定義
└── your-app-test.asd     # テストシステム定義
```

### テストシステム定義

テストシステムは`your-app-test.asd`で定義されています：

```common-lisp
(defsystem your-app-test
  :class :package-inferred-system
  :pathname "test"
  :depends-on ("clails"
               "rove"
               "your-app"
               "your-app-test/test-loader")
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))
```

---

## 2. テストの作成

### deftest-suiteを使用した基本的なテスト構造

clailsはRoveを`deftest-suite`マクロで拡張し、テストにタグを付けることができます：

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/models/user
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:your-app-test/models/user)

(deftest-suite :model test-user-creation
  (testing "新しいユーザーを作成"
    (let ((user (make-record '<user> :name "Alice")))
      (ok (save user) "ユーザーが正常に保存された")
      (ok (ref user :id) "保存後にユーザーがIDを持つ"))))

(deftest-suite (:model :validation) test-user-validation
  (testing "ユーザーバリデーション"
    (let ((user (make-record '<user> :name "")))
      (ok (not (save user)) "空の名前はバリデーションに失敗する")
      (ok (has-error-p user) "ユーザーにバリデーションエラーがある"))))
```

### deftest-suite 構文

```common-lisp
(deftest-suite tags test-name
  body...)
```

- **tags**: 単一のキーワード (`:model`) またはキーワードのリスト (`(:model :validation)`)
- **test-name**: テストに名前を付けるシンボル
- **body**: Roveアサーションを使用したテストコード

### サンプルテストファイル

新しいプロジェクトを作成すると、`test/sample.lisp`にサンプルテストファイルが生成されます：

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/sample
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:your-app-test/sample)

(deftest-suite :sample sample-basic-test
  (testing "サンプル基本テスト"
    (ok t "常に成功")))

(deftest-suite (:sample :number) sample-number-test
  (testing "サンプル数値テスト"
    (ok (= 1 1) "1は1と等しい")
    (ok (> 2 1) "2は1より大きい")))

(deftest-suite (:sample :string) sample-string-test
  (testing "サンプル文字列テスト"
    (ok (stringp "hello world") "文字列は文字列型")
    (ok (string= "hello" "hello") "文字列が等しい")))
```

---

## 3. Roveアサーション

clailsはRoveのアサーション関数を使用します：

### ok - 式が真であることをアサート

```common-lisp
(ok (= 2 (+ 1 1))
    "1 + 1は2と等しい")
```

### ng - 式が偽であることをアサート

```common-lisp
(ng (string= "hello" "world")
    "文字列は等しくない")
```

### signals - コードがエラーをシグナルすることをアサート

```common-lisp
(ok (signals (error "テストエラー"))
    "エラーをシグナルする")
```

### testing - 関連するアサーションをグループ化

```common-lisp
(testing "ユーザー作成"
  (ok (create-user "Alice"))
  (ok (find-user-by-name "Alice")))
```

---

## 4. テストの実行

### すべてのテストを実行

プロジェクトディレクトリから：

```bash
clails test
```

これにより、アプリケーション内のすべてのテストが実行されます。

### パッケージでテストを実行

特定のパッケージのテストを実行：

```bash
clails test your-app-test/models/user
clails test your-app-test/controllers/user-controller
```

複数のパッケージ：

```bash
clails test your-app-test/models/user your-app-test/models/post
```

### タグでテストを実行

特定のタグを持つすべてのテストを実行：

```bash
clails test --tag model
```

複数のタグでテストを実行：

```bash
clails test --tag model --tag validation
```

### タグでテストを除外

遅いテストや特定のテストを除外：

```bash
clails test --exclude slow
clails test --exclude integration
```

### フィルタの組み合わせ

パッケージとタグのフィルタを組み合わせることができます：

```bash
clails test your-app-test/models --tag validation
clails test --tag model --exclude slow
```

---

## 5. テスト検索とリスト表示

### 利用可能なすべてのタグをリスト表示

```bash
clails test --list-tags
```

出力：
```
Available tags:
  :CONTROLLER
  :MODEL
  :SAMPLE
  :STRING
  :NUMBER
  :VALIDATION
```

### すべてのテストパッケージをリスト表示

```bash
clails test --list-packages
```

出力：
```
Available packages:
  YOUR-APP-TEST/SAMPLE
  YOUR-APP-TEST/MODELS/USER
  YOUR-APP-TEST/CONTROLLERS/USER-CONTROLLER
```

### 特定のタグを持つテストをリスト表示

```bash
clails test --list-tests-tag model
```

出力：
```
Tests with tag :MODEL:
  TEST-USER-CREATION (YOUR-APP-TEST/MODELS/USER)
  TEST-USER-VALIDATION (YOUR-APP-TEST/MODELS/USER)
  TEST-POST-CREATION (YOUR-APP-TEST/MODELS/POST)
```

### 特定のパッケージ内のテストをリスト表示

```bash
clails test --list-tests-pkg your-app-test/models/user
```

出力：
```
Tests in package YOUR-APP-TEST/MODELS/USER:
  TEST-USER-CREATION [:MODEL]
  TEST-USER-VALIDATION [:MODEL :VALIDATION]
```

---

## 6. テストの生成

### モデルテストの生成

モデルを生成すると、対応するテストファイルが作成されます：

```bash
clails generate:model user
```

これにより以下が作成されます：
- `app/models/user.lisp` - モデルファイル
- `test/models/user.lisp` - テストファイル

生成されたテストファイル：

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/models/user
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:your-app-test/models/user)

(deftest-suite :model test-user-model
  (testing "Test user model"
    (ok (= 1 0) "This test should be replaced with actual test")))
```

### コントローラーテストの生成

コントローラーを生成する場合：

```bash
clails generate:controller user
```

これにより以下が作成されます：
- `app/controllers/user-controller.lisp` - コントローラーファイル
- `test/controllers/user-controller.lisp` - テストファイル

---

## 7. モデルのテスト

### モデルテストの例

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/models/user
  (:use #:cl
        #:rove
        #:clails/test
        #:your-app/models/user))
(in-package #:your-app-test/models/user)

(deftest-suite :model test-user-save
  (testing "新しいユーザーを保存"
    (let ((user (make-record '<user> 
                  :name "Alice"
                  :email "alice@example.com")))
      (ok (save user)
          "ユーザーが正常に保存される")
      (ok (ref user :id)
          "保存後にユーザーがIDを持つ")
      (ok (string= (ref user :name) "Alice")
          "ユーザー名が保持される"))))

(deftest-suite (:model :query) test-user-query
  (testing "ユーザーをクエリ"
    (let ((users (execute-query 
                   (query <user>
                          :as :user
                          :where (:= (:user :name) :name))
                   '(:name "Alice"))))
      (ok (> (length users) 0)
          "Aliceという名前のユーザーが見つかった"))))

(deftest-suite (:model :validation) test-user-validation
  (testing "ユーザーバリデーション"
    (let ((user (make-record '<user> :name "" :email "")))
      (ng (save user)
          "空の名前とメールは失敗する")
      (ok (has-error-p user)
          "ユーザーにバリデーションエラーがある")
      (ok (ref-error user :name)
          "名前エラーが設定されている")
      (ok (ref-error user :email)
          "メールエラーが設定されている"))))
```

---

## 8. コントローラーのテスト

### コントローラーテストの例

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/controllers/user-controller
  (:use #:cl
        #:rove
        #:clails/test))
(in-package #:your-app-test/controllers/user-controller)

(deftest-suite :controller test-user-controller-list
  (testing "ユーザーをリスト表示"
    ;; コントローラーロジックをテスト
    (ok t "コントローラーテストのプレースホルダー")))
```

---

## 9. テスト整理のベストプラクティス

### タグでテストを整理

タグを使用してテストを分類します：

- `:model` - モデル/データベーステスト
- `:controller` - コントローラーテスト
- `:view` - ビューレンダリングテスト
- `:integration` - 統合テスト
- `:unit` - ユニットテスト
- `:slow` - 実行速度が遅いテスト
- `:validation` - バリデーションテスト

### タグの使用例

```common-lisp
;; ユニットテスト - 高速、分離
(deftest-suite (:model :unit) test-user-name-formatting
  (testing "ユーザー名をフォーマット"
    (ok (string= (format-name "alice") "Alice"))))

;; 統合テスト - 遅い、データベースを使用
(deftest-suite (:model :integration) test-user-with-posts
  (testing "投稿を持つユーザー"
    (let ((user (find-user-by-id 1)))
      (ok (> (length (ref user :posts)) 0)))))

;; 遅いテスト - 高速実行時に除外するためのマーク
(deftest-suite (:model :slow) test-bulk-user-creation
  (testing "1000人のユーザーを作成"
    (ok (create-many-users 1000))))
```

### 推奨されるテストファイル構造

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/models/user
  (:use #:cl
        #:rove
        #:clails/test
        #:your-app/models/user))
(in-package #:your-app-test/models/user)

;; セットアップ - テスト前に実行
(setup
  ;; テストデータ、データベースなどを初期化
  )

;; ティアダウン - テスト後に実行
(teardown
  ;; テストデータをクリーンアップ
  )

;; 機能ごとにグループ化されたテスト
(deftest-suite :model test-user-creation
  ...)

(deftest-suite :model test-user-update
  ...)

(deftest-suite :model test-user-deletion
  ...)

(deftest-suite (:model :validation) test-user-validation
  ...)
```

---

## 10. テストローダー設定

`test/test-loader.lisp`ファイルは、すべてのテストモジュールがロードされることを保証します：

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/test-loader
  (:use #:cl)
  (:import-from #:your-app-test/sample)
  (:import-from #:your-app-test/models/user)
  (:import-from #:your-app-test/controllers/user-controller))
(in-package #:your-app-test/test-loader)
```

**重要**: テストを実行する際にロードされるように、作成した新しいテストファイルごとにインポートを追加してください。

---

## 11. コマンドリファレンス

### clails test

オプションのフィルタリングでテストを実行します。

```bash
clails test [PACKAGES...] [OPTIONS]
```

#### 引数

- `PACKAGES...` - テストするパッケージ名（完全一致）

#### オプション

- `--tag TAG` - TAGを持つテストを含める（複数回指定可能）
- `--exclude TAG` - TAGを持つテストを除外（複数回指定可能）
- `--list-tags` - 利用可能なすべてのタグをリスト表示
- `--list-packages` - 利用可能なすべてのパッケージをリスト表示
- `--list-tests-tag TAG` - 特定のタグを持つテストをリスト表示
- `--list-tests-pkg PKG` - 特定のパッケージ内のテストをリスト表示
- `-h, --help` - ヘルプメッセージを表示

#### 例

```bash
# すべてのテストを実行
clails test

# 特定のパッケージでテストを実行
clails test your-app-test/models/user
clails test your-app-test/models/user your-app-test/models/post

# 特定のタグでテストを実行
clails test --tag model

# 複数のタグでテストを実行
clails test --tag model --tag validation

# 遅いテストを除外
clails test --exclude slow

# フィルタを組み合わせる
clails test --tag model --exclude slow
clails test your-app-test/models --tag validation

# 利用可能なタグをリスト表示
clails test --list-tags

# 利用可能なパッケージをリスト表示
clails test --list-packages

# 特定のタグを持つテストをリスト表示
clails test --list-tests-tag model

# 特定のパッケージ内のテストをリスト表示
clails test --list-tests-pkg your-app-test/models/user
```

---

## 12. ベストプラクティス

### 説明的なテスト名を書く

```common-lisp
;; 良い
(deftest-suite :model test-user-validates-email-format
  ...)

;; 不明確
(deftest-suite :model test1
  ...)
```

### 意味のあるアサーションメッセージを使用

```common-lisp
;; 良い
(ok (string= (ref user :name) "Alice")
    "ユーザー名は'Alice'である")

;; 役に立たない
(ok (string= (ref user :name) "Alice"))
```

### テストに適切にタグを付ける

```common-lisp
;; コンポーネントでタグ付け
(deftest-suite :model ...)
(deftest-suite :controller ...)

;; テストタイプでタグ付け
(deftest-suite :unit ...)
(deftest-suite :integration ...)

;; 柔軟性のために複数のタグ
(deftest-suite (:model :validation :slow) ...)
```

### テストを独立させる

各テストは独立して実行できる必要があります：

```common-lisp
;; 良い - 各テストが独自のデータを作成
(deftest-suite :model test-user-creation
  (let ((user (make-record '<user> :name "Test")))
    (ok (save user))))

(deftest-suite :model test-user-deletion
  (let ((user (make-record '<user> :name "Test")))
    (save user)
    (ok (destroy user))))
```

### セットアップとティアダウンを使用

初期化を共有するテストの場合：

```common-lisp
(setup
  (clails/model/connection:startup-connection-pool)
  (seed-test-data))

(teardown
  (clean-test-data)
  (clails/model/connection:shutdown-connection-pool))
```

### 成功と失敗の両方のケースをテスト

```common-lisp
(deftest-suite :model test-user-validation
  (testing "有効なユーザー"
    (let ((user (make-record '<user> 
                  :name "Alice" 
                  :email "alice@example.com")))
      (ok (save user) "有効なユーザーが保存される")))
  
  (testing "無効なユーザー - 空の名前"
    (let ((user (make-record '<user> 
                  :name "" 
                  :email "alice@example.com")))
      (ng (save user) "空の名前はバリデーションに失敗")))
  
  (testing "無効なユーザー - 無効なメール"
    (let ((user (make-record '<user> 
                  :name "Alice" 
                  :email "not-an-email")))
      (ng (save user) "無効なメールはバリデーションに失敗"))))
```

---

## まとめ

clailsはアプリケーションのための包括的なテストフレームワークを提供します：

- **deftest-suite** - 簡単なフィルタリングのためのタグ付きテストを定義
- **clails test** - 柔軟なフィルタリングオプションでテストを実行
- **タグとパッケージ** - 効率的な実行のためにテストを整理
- **自動生成** - モデルとコントローラーと共にテストファイルを生成
- **Rove統合** - 使い慣れたRoveアサーションと構文を使用

このガイドとベストプラクティスに従うことで、clailsアプリケーションの品質と信頼性を保証する効果的なテストを書くことができます。
