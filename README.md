fpinscala-with-spec2
====================

fpinscalaの勉強会で使うspec2のひな形です。
IntelliJ Idea 12でやっています。

* scalaとgradleはインストール済み
* IntelliJでscala pluginとgradleが有効になっている。

* プロジェクトのルートディレクトリで下記を実行。
```
gradle idea
```

* JetGradleでbuild.gradleを選択してプロジェクトを同期させる。

* project structure -> Modules -> Scalaを選択 -> Compiler Libraryのところでgradle-scala-compiler-2.10.3を選択。

* あとはspecファイルを右クリック-> Run 'HogeSpec'で実行できます。
