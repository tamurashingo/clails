# clails テストガイド

## 概要

clailsのテストフレームワークは、シンプルで表現力豊かなテスト構文を提供するCommon Lispテストフレームワーク **Rove** を使用しています。
テストはコンポーネント（model、controller、view、helperなど）ごとに整理され、複数のデータベースをサポートするDockerベースの環境で実行できます。

## 基本概念

- テストはRoveテストフレームワークを使用
- テストファイルはソースコードの構造をミラーリング（例：モデルテストは `test/model/`）
- 複数のデータベースバックエンド（SQLite3、MySQL、PostgreSQL）をサポート
- Dockerベースのテスト環境により一貫性と分離を確保
- Makefileコマンドでテスト実行を簡素化

---

## 1. テスト構造

### ファイル構成

テストは `test/` ディレクトリにソースコードと同じ構造で整理されています：

```
test/
├── controller/         # コントローラーテスト
├── helper/             # ヘルパー関数テスト
├── model/              # モデルとデータベーステスト
├── view/               # ビューテンプレートテスト
├── datetime/           # DateTimeユーティリティテスト
├── logger/             # ロガーテスト
├── e2e/                # エンドツーエンド統合テスト
└── util.lisp           # テストユーティリティ
```

### テストファイルの命名規則

テストファイルはテストするソースファイルと同じ名前に従います：

- ソース: `src/model/query.lisp` → テスト: `test/model/query.lisp`
- ソース: `src/helper/date-helper.lisp` → テスト: `test/helper/date-helper.lisp`

---

## 2. Roveでテストを書く

### 基本的なテスト構造

```common-lisp
(in-package #:cl-user)
(defpackage #:your-app-test/component-name
  (:use #:cl
        #:rove
        #:your-app/component-name))

(in-package #:your-app-test/component-name)

(deftest test-name
  (testing "テストケースの説明"
    (ok (= 1 1)
        "1は1と等しい")
    (ng (= 1 2)
        "1は2と等しくない")))
```

### Roveのアサーション

Roveはいくつかのアサーション関数を提供します：

#### `ok` - 式が真であることをアサート

```common-lisp
(ok (= 2 (+ 1 1))
    "1 + 1は2と等しい")
```

#### `ng` - 式が偽であることをアサート

```common-lisp
(ng (= 3 (+ 1 1))
    "1 + 1は3と等しくない")
```

#### `signals` - コードがエラーをシグナルすることをアサート

```common-lisp
(ok (signals
      (error "テストエラー"))
    "エラーをシグナルする")
```

### セットアップとティアダウン

テストの初期化とクリーンアップには `setup` と `teardown` を使用します：

```common-lisp
(setup
  ;; 初期化コード
  (initialize-database)
  (seed-test-data))

(teardown
  ;; クリーンアップコード
  (cleanup-database))

(deftest my-test
  ;; テストはsetupとteardownの間で実行されます
  (ok (test-something)))
```

---

## 3. テストの実行

### 前提条件

テストを実行する前に、以下がインストールされていることを確認してください：

- DockerとDocker Compose
- Makeユーティリティ

### すべてのテストを実行

すべてのテストをビルドして実行：

```bash
make test.build
make test
```

これにより：
1. テストDockerイメージをビルド
2. データベースコンテナ（MySQL、PostgreSQL、SQLite3）を起動
3. qlotで依存関係をインストール
4. Roveを使用してすべてのテストを実行

### テスト環境のクリーンアップ

テストコンテナとイメージをクリーンアップ：

```bash
make test.down    # データベースコンテナを停止
make test.clean   # コンテナとボリュームを削除
```

---

## 4. モデルのテスト

モデルテストは通常、データベース操作を伴い、データベースのセットアップが必要です。

### モデルテストの例

```common-lisp
(in-package #:cl-user)
(defpackage #:clails-test/model/save
  (:use #:cl
        #:rove
        #:clails/model/query)
  (:import-from #:clails/model/base-model
                #:<base-model>
                #:defmodel
                #:ref
                #:save
                #:destroy))

(in-package #:clails-test/model/save)

(setup
  ;; テストモデルを定義
  (defmodel <user> (<base-model>)
    (:table "users"))
  
  ;; データベースを初期化
  (initialize-database)
  (migrate-database))

(teardown
  (cleanup-database))

(deftest save-record-test
  (testing "新しいレコードの保存"
    (let ((user (make-record '<user> :name "John")))
      (ok (save user)
          "正常に保存される")
      (ok (ref user :id)
          "保存後にレコードがIDを持つ"))))
```

### データベース設定

環境変数を使用して、異なるデータベース用にテストを設定できます：

```common-lisp
(setf clails/environment:*database-type* 
      (make-instance 'clails/environment::<database-type-mysql>))

(setf clails/environment:*database-config* 
      `(:test (:database-name ,(env-or-default "CLAILS_MYSQL_DATABASE" "clails_test")
               :username ,(env-or-default "CLAILS_MYSQL_USERNAME" "root")
               :password ,(env-or-default "CLAILS_MYSQL_PASSWORD" "password")
               :host ,(env-or-default "CLAILS_MYSQL_HOST" "mysql-test")
               :port ,(env-or-default "CLAILS_MYSQL_PORT" "3306"))))
```

---

## 5. コントローラーのテスト

コントローラーテストは、リクエスト処理、パラメータ処理、レスポンス生成を検証します。

### コントローラーテストの例

```common-lisp
(in-package #:cl-user)
(defpackage #:clails-test/controller/base-controller
  (:use #:cl
        #:rove
        #:clails/controller/base-controller))

(in-package #:clails-test/controller/base-controller)

(defclass <test-controller> (<base-controller>)
  ())

(deftest path-matching-test
  (testing "ルートパスにマッチ"
    (let ((result (match-path "/")))
      (ok result
          "ルートパスにマッチする")))
  
  (testing "パラメータ付きパスにマッチ"
    (let ((result (match-path "/users/123")))
      (ok result
          "パラメータ付きパスにマッチする")
      (ok (string= (getf result :id) "123")
          "パラメータを正しく抽出する"))))
```

---

## 6. ビューのテスト

ビューテストは、テンプレートの解析、コンパイル、レンダリングを検証します。

### ビューテストの例

```common-lisp
(in-package #:cl-user)
(defpackage #:clails-test/view/renderer
  (:use #:cl
        #:rove
        #:clails/view/renderer))

(in-package #:clails-test/view/renderer)

(deftest render-template-test
  (testing "シンプルなテンプレートのレンダリング"
    (let ((result (render-string "<p>Hello, <%= name %>!</p>"
                                  :name "World")))
      (ok (string= result "<p>Hello, World!</p>")
          "変数を含むテンプレートをレンダリングする"))))
```

---

## 7. ヘルパーのテスト

ヘルパーテストは、ユーティリティ関数とヘルパーメソッドを検証します。

### ヘルパーテストの例

```common-lisp
(in-package #:cl-user)
(defpackage #:clails-test/helper/date-helper
  (:use #:cl
        #:rove
        #:clails/helper/date-helper))

(in-package #:clails-test/helper/date-helper)

(deftest datetime-format-test
  (let ((ut (encode-universal-time 45 34 13 02 01 1998)))
    (ok (string= (view/datetime ut)
                 "1998/01/02 13:34:45")
        "デフォルトフォーマットで日時をフォーマットする")
    
    (ok (string= (view/datetime ut :fmt "%Y")
                 "1998")
        "カスタムフォーマットで日時をフォーマットする")))
```

---

## 8. エンドツーエンド（E2E）テスト

E2Eテストは、完全なアプリケーションを作成してテストすることで、アプリケーション全体のワークフローを検証します。

### E2Eテストの実行

```bash
make e2e.build   # E2Eテストイメージをビルド
make e2e.test    # E2Eテストを実行
make e2e.clean   # E2E環境をクリーンアップ
```

### E2Eテスト構造

E2Eテストは `test/e2e/` に定義されており、以下を含みます：

- `todo-app-e2e.sh` - 完全なアプリケーションを作成してテストするシェルスクリプト
- `templates/` - テストアプリケーション用のテンプレートファイル

E2Eテストは：
1. 新しいclailsアプリケーションを作成
2. スキャフォールディングを生成
3. マイグレーションを作成してデータをシード
4. アプリケーションを実行
5. 機能を検証

---

## 9. データベース固有のテスト

### SQLite3でのテスト

```bash
make test.sqlite3
```

これによりテストデータベースに接続されたSQLite3コンソールが開きます。

### MySQLでのテスト

```bash
make test.mysql
```

これによりテストデータベースに接続されたMySQLコンソールが開きます。

### PostgreSQLでのテスト

```bash
make test.postgresql
```

これによりテストデータベースに接続されたPostgreSQLコンソールが開きます。

---

## 10. 対話型テストコンソール

デバッグと対話型テストのため：

```bash
make test.console
```

これによりテストコンテナ内のbashシェルが開き、以下が可能になります：
- 個別のテストを実行
- テスト環境を検査
- テストの失敗をデバッグ
- コードを実験

コンソール内で、特定のテストを実行できます：

```bash
qlot exec rove test/model/save.lisp
```

---

## 11. テストシステム定義

テストは `clails-test.asd` で定義されています：

```common-lisp
(defsystem clails-test
  :class :package-inferred-system
  :pathname "test"
  :depends-on (#:babel
               #:clails
               #:rove
               #:clails-test/util
               #:clails-test/model/impl/sqlite3
               #:clails-test/model/impl/mysql
               #:clails-test/model/impl/postgresql
               #:clails-test/model/connection
               #:clails-test/model/query
               #:clails-test/controller/base-controller
               #:clails-test/helper/date-helper
               #:clails-test/view/parser
               #:clails-test/view/compiler
               #:clails-test/view/renderer
               #:clails-test/logger/registry
               #:clails-test/datetime/all
               ;; ... その他多くのテストモジュール
               )
  :perform (test-op (o c)
             (uiop:symbol-call :rove :run c)))
```

新しいテストを追加するには、`:depends-on` リストに含めます。

---

## 12. ベストプラクティス

### 分離

- 各テストは独立している必要があります
- クリーンな状態を確保するために `setup` と `teardown` を使用
- 実行順序に依存しない

### 説明的なテスト名

```common-lisp
;; 良い
(deftest user-validation-requires-email
  ...)

;; 不明確
(deftest test1
  ...)
```

### エッジケースのテスト

ハッピーパスだけでなく、以下もテスト：
- 空の入力
- Nil値
- 境界条件
- エラー条件

### 意味のあるアサーションを使用

```common-lisp
;; 良い
(ok (= (length users) 3)
    "正確に3人のユーザーを返す")

;; 役に立たない
(ok (= (length users) 3))
```

### データベーステスト

- トランザクションを使用して変更をロールバック
- SQLiteデータベースには一時ディレクトリを使用
- `teardown` でテストデータをクリーンアップ

### 外部依存関係をモック

外部サービスに依存するコンポーネントをテストする場合、以下のためにモックを検討：
- テスト速度の向上
- テストの信頼性の確保
- 副作用の回避

---

## 13. 継続的インテグレーション

テストはDockerを使用してCI環境で実行されるように設計されており、以下を保証します：
- マシン間で一貫したテスト環境
- 分離されたデータベースインスタンス
- 再現可能なテスト結果

テスト設定はdocker-composeを使用して複数のデータベースコンテナを調整し、CIパイプラインに適しています。

---

## まとめ

clailsテストフレームワークは以下を提供します：

- **Rove** による表現力豊かで読みやすいテスト
- **Dockerベース** の環境による一貫性
- **マルチデータベース** サポートによる包括的なテスト
- **Makeコマンド** による簡単なテスト実行
- **E2Eテスト** による完全なアプリケーション検証
- **対話型コンソール** によるデバッグ

これらのガイドラインとパターンに従うことで、clailsアプリケーションの品質と信頼性を保証する効果的なテストを書くことができます。
