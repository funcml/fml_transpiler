# FunML - A (Fun)ctional Markup Language!

<img src="/assets/images/funml.png" alt="funml" width="200"/>

## Project Overview

**FunML (Functional Markup Language)** is an innovative markup language designed to bring the power and expressiveness of functional programming concepts to document authoring and data representation. FunML enables users to write concise, readable, and highly composable markup that can be transformed and processed with functional paradigms.

### Milestone
- [x] Parsing Components and Tags
- [x] Parsing Attributes
- [x] Parsing Props
- [x] Parsing State

### Syntax Examples

Here are some basic FunML syntax examples:

```funml
script (
  import { createSignal } from '@lib';
  
  const [count, setCount] = createSignal(0);
)

Counter => (
  div class="p-4 bg-gray-100 rounded-lg text-center" (
    h1 class="text-2xl font-bold mb-4" $ "Hello world",
    p class="" $ [() => `Counter: ${count()}`],
    button onclick=[() => setCount(prev => prev + 1)] $ "Increment"
  )
)
```

### Getting Started

1. Clone repository ini

2. Jalankan ```build.sh```

3. Hasil binary akan ada di ```/bin```


### 📚 Lesson Learnt
##### Parser Combinator

Untuk melakukan parsing dan transpilasi, kita menggabungkan berbagai macam instance dari type Parser berikut untuk consume string input jika pattern tertentu terpenuhi.

```haskell
ifBlock :: Parser FMLElement
ifBlock = do
  _ <- operator "@if"
  cond <- trim <$> readUntilKeyword "then"
  _ <- operator "then"
  firstEl <- branchBody
  elifs <- zeroOrMore elifBlock
  finalElse <- optional elseBlock
  whitespaces
  _ <- operator "@end"
  _ <- operator "if"
  return $ FMLGuards ((cond, firstEl) : elifs) finalElse
```
Kode ifBlock di atas adalah contoh nyata penerapan Parser Combinator. Alih-alih menulis satu fungsi raksasa untuk memproses seluruh string, kita menggabungkan (kombinasi) parser-parser kecil yang spesifik. Seperti operator (untuk mencocokkan kata kunci "@if", "then"), readUntilKeyword (untuk membaca kondisi), zeroOrMore (untuk menangani banyak elif), dan optional (untuk else). Semua digabung menjadi satu parser kompleks yang mampu mengenali struktur logika percabangan secara utuh.



##### “Power” dari Monad

```haskell
{-# LANGUAGE DeriveFunctor #-}

module FML.Lib.Parser where

import FML.Grammar (ParseError, Position)

newtype Parser a = Parser {runParser :: (String, Position) -> (String, Position, Either ParseError a)}
  deriving (Functor)

instance Applicative Parser where
  pure c = Parser $ \(s, pos) -> (s, pos, Right c)
  pf <*> pa = Parser $ \(s, pos) -> case runParser pf (s, pos) of
    (s', pos', Right f) -> case runParser pa (s', pos') of
      (s'', pos'', Right a) -> (s'', pos'', Right (f a))
      (s'', pos'', Left e) -> (s'', pos'', Left e)
    (s', pos', Left e) -> (s', pos', Left e)

instance Monad Parser where
  pa >>= f = Parser $ \(s, pos) -> case runParser pa (s, pos) of
    (s', pos', Right a) -> runParser (f a) (s', pos')
    (s', pos', Left e) -> (s', pos', Left e)
```

Kode `instance Monad Parser` di atas mendefinisikan bagaimana dua parser digabungkan secara berurutan. "Kekuatan" utamanya terletak pada operator `>>=` (bind).

Secara internal, operator ini melakukan dua tugas repetitif secara otomatis:
1. Penggabungan output parser yang pertama ke input parser selanjutnya (komposisi parser). Ia mengambil state `(s', pos')` (sisa string dan posisi kursor) dari parser pertama dan mengopernya ke parser kedua.
2. Early stop jika ada error saat parsing. Jika parser pertama gagal (`Left e`), ia langsung menghentikan proses dan mengembalikan error tersebut tanpa menjalankan parser kedua.

### Jika Ditulis Tanpa Monad (Pendekatan Imperatif)

Jika kita tidak menggunakan Monad (misalnya dalam bahasa C atau Java gaya lama tanpa exception handling untuk control flow), kita harus mengelola state string dan pengecekan error secara manual di setiap langkah.

Bayangkan kita ingin memparsing urutan: `operator "@if"` lalu `readUntilKeyword "then"`.

Kode akan penuh dengan "boilerplate" untuk mengecek error dan mengupdate posisi kursor.

```c
// Kita harus melempar state (input, pos) dan error secara manual
Result parseIfBlock(String input, int pos) {
    // 1. Coba parse "@if"
    Result res1 = parseOperator(input, pos, "@if");
    
    // Pengecekan Error Manual #1
    if (res1.isError) {
        return res1; // Gagal, return error segera
    }
    
    // Update state manual
    int newPos = res1.newPos;
    
    // 2. Coba parse kondisi
    Result res2 = readUntilKeyword(input, newPos, "then");
    
    // Pengecekan Error Manual #2
    if (res2.isError) {
        return res2; // Gagal
    }
    
    // Update state lagi
    newPos = res2.newPos;
    String cond = res2.value;
    
    // ... dan seterusnya untuk setiap baris parser ...
    
    return Success(cond, ...);
}
```

**Masalah pada pendekatan imperatif di atas:**
1.  Deep Nesting / Early Return, Kita harus selalu mengecek `if (error)` setelah setiap operasi.
2.  State Management, variabel `newPos` harus terus di-update dan di-pass ke fungsi berikutnya secara manual. Sangat rawan bug (misal: tidak sengaja mengirim `pos` lama ke fungsi baru).

**Dengan Monad (Haskell):**
Semua kerumitan di atas diabstraksi.

```haskell
ifBlock = do
  _    <- operator "@if"              -- State passing & error check otomatis
  cond <- readUntilKeywordifBlock = do
  _    <- operator "@if"              -- State passing & error check otomatis
  cond <- readUntilKeyword "then"     -- State passing & error check otomatis
  return cond
```

Monad mengabstraksi "boilerplate" tersebut sehingga developer hanya fokus pada logika grammar, bukan pada mekanika passing variabel dan manajemen state serta exception.

##### Balanced Parentheses / Bracket Parsing

- Problem: Misalkan input berikut, “[ ]” digunakan untuk menandakan javascript expression. Namun javascript expression sendiri bisa mengandung “[ ]“. Permasalahan terjadi ketika parser tidak tahu closing bracket “]” itu milik scope js expression atau merupakan string dari kode js.

```haskell
balancedBracketContent :: Int -> Parser String
balancedBracketContent 0 = return ""
balancedBracketContent level = do
  c <- satisfyCond "any character" (const True)
  case c of
    ']' -> (c :) <$> balancedBracketContent (level - 1)
    '[' -> (c :) <$> balancedBracketContent (level + 1)
    _ -> (c :) <$> balancedBracketContent level
```
- Solution: Proses parsing tidak dapat dilakukan secara greedy dari kiri ke kanan. Parsing dilakukan dari “[ ]“ terluar hingga terdalam secara rekursif.

Kode `balancedBracketContent` merupakan parser rekursif yang dirancang untuk membaca konten di dalam kurung siku, termasuk menangani kasus kurung bersarang (*nested brackets*). Fungsi ini bekerja dengan melacak kedalaman struktur menggunakan parameter `level`; setiap kali menemukan kurung buka `[`, kedalaman bertambah, dan sebaliknya berkurang saat bertemu kurung tutup `]`. Proses pembacaan karakter terus berlanjut hingga `level` mencapai 0, yang menandakan bahwa kurung pembuka paling luar telah tertutup sempurna, sehingga parser dapat membedakan antara kurung penutup milik struktur dalam dengan kurung penutup akhir dari blok tersebut.


- Hal ini karena syntax ini tidak termasuk kedalam grammar CFG sehingga perlu handling khusus menggunakan stack (stack diimplementasikan secara implisit lewat rekursi).

##### Design Syntax

- Dari pembuatan parser untuk templating language ini, saya mendapati mengapa format syntax HTML/JSX cukup populer. Tidak hanya karena format ini umum digunakan, tapi juga proses parsing menjadi jauh lebih sederhana. Hal ini karena start dan end serta delimiter pada syntax HTML/JSX memiliki ambiguitas yang lebih sedikit. Hal ini menurunkan jumlah edge cases serta kompleksitas dari parser.

- Jadi lesson learned disini adalah banyak sekali edge case yang perlu dipikirkan saat merancang syntax. Dan perlu dipertimbangkan juga apakah solusi baru itu lebih baik dibanding solusi yang sudah ada. Dari sini juga terlihat terdapat beberapa design choice  “hidden” pada bahasa yang biasa kita gunakan  serta compromise antara kompleksitas parsing/compiling dengan developer experience.

##### Manual vs Automatic Parsing

- Selama ini kita menggunakan manual top-down parsing dengan sebagian besar berupa algoritma recursive descent parsing
- Namun ada juga alternatif lain seperti algoritma automatic parsing seperti LALR yang merupakan algoritma bottom-up parsing yang secara otomatis menghasilkan parser dari input berupa grammar.
- Pada minggu ini kita mencoba menggunakan parser bernama lezer yang merupakan sebuah LR(1) dan pseudo-LALR ketika kita mencoba mengimplementasikan syntax highlighting di codemirror code editor di REPL.
- Kita hanya perlu menuliskan grammar dan regex tokenizer dan secara otomatis parser akan dihasilkan.

###### Manual parser (terutama RD parser)
- Bersifat top-down (lebih intuitif). bisa dengan mudah mengimplementasikan dan menyelesaikan beberapa masalah seperti nested brackets atau paired bracket dan syntax if/else.
- Lebih mudah di debug dan juga lebih mudah diatur output errornya.
- Dapat mengakomodasi lebih banyak jenis grammar dan bahkan jika ada beberapa grammar set yang berbeda dalam satu bahasa (ada LR(1), LL(K), context-free, context-sensitive)

###### Automatic parser seperti LALR,
- Lebih mudah ditulis tapi tidak fleksibel. Hanya menerima grammar yang spesifik seperti LL(1) saja.
- Digunakan untuk syntax highlighting karena dengan ini seorang developer dapat dengan mudah membuat extensi highlighting sendiri tanpa mengimplementasikan parser sendiri.
- Selain itu digunakan untuk syntax highligting karena proses ini lebih sederhana dan butuh konteks yang lebih sedikit (tidak perlu tahu itu if/else block hanya perlu tahu itu keyword, argumen function, dst.)

##### Apakah bisa bahasa kompleks di parse dengan automatic parser?

- Jawabanya Iya! Namun, perlu tambahan lagi yaitu semantic actions.

- Jadi semantic actions adalah simbol yang disisipkan di AST sebagai “penanda” setelah suatu action di detect ketika parsing, logic tambahan dilakukan.

Misal
```
Branch → “if” C1 expr “then” C2 expr C3 “end” “if” C4
```

- Sistemnya seperti interrupt di OS. Setiap C1, C2, ... akan trigger suatu action/kode yang menghandle verifikasi, symbol lookup, dst.



Note: Go [here](https://github.com/funcml/funcml_runtime/blob/main/README.md) to see our lesson learnt in the other aspect (the runtime side)

### Contributing

We welcome contributions! Please see [CONTRIBUTING.md](./CONTRIBUTING.md) for guidelines.

### License

FunML is released under the MIT License.

---
For more information, visit the [project website](https://github.com/pemfung/funml) or join the discussion on our [community forum](https://community.funml.dev).

