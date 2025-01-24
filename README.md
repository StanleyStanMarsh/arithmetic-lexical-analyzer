# EN - Binary Parser

This project is a parser that reads lines from a `.txt` file containing values and binary operations. Supported operations include AND (&), OR (|), and XOR (^). The project is written in Haskell and uses the `Lib.hs` library for pure functions and helper operations.

## Formal Parser Description

The parser is implemented as a deterministic finite automaton (DFA) that recognizes lines such as `  10101   |101   `.

![Finite Automaton Diagram](automata_bin_parser.png)

*In the graph, operation = {&, |, ^} represents the set of valid binary operations.*

### Formal Definition

M = (Q, Σ, δ, q₀, F), where:

- Q = {s₀, s₁, s₂, s₃, s₄, s₅, s₆, s₇} - set of states
- Σ = {0, 1, space, &, |, ^, other, EOL} - input alphabet
- q₀ = s₀ - initial state
- F = {s₆} - set of accepting states
- δ : Q × Σ → Q - transition function, defined in the following table:

| State    | space | 0,1  | &,\|,^ | other | EOL  |
|----------|-------|-------|--------|-------|------|
| s₀       | s₀    | s₁    | s₇     | s₇    | s₇   |
| s₁       | s₂    | s₁    | s₇     | s₇    | s₇   |
| s₂       | s₂    | s₇    | s₃     | s₇    | s₇   |
| s₃       | s₃    | s₄    | s₇     | s₇    | s₇   |
| s₄       | s₅    | s₄    | s₇     | s₇    | s₇   |
| s₅       | s₅    | s₇    | s₇     | s₇    | s₆   |
| s₆       | s₇    | s₇    | s₇     | s₇    | s₇   |
| s₇       | s₇    | s₇    | s₇     | s₇    | s₇   |

### State Semantics:
- s₀: Initial state, awaiting the first binary number
- s₁: Reading the first binary number
- s₂: Awaiting a binary operation
- s₃: Awaiting the second binary number
- s₄: Reading the second binary number
- s₅: Awaiting the end of the line
- s₆: Accepting state
- s₇: Error state

### Notes:
- The automaton accepts lines in the format: `[spaces][binary number][spaces][operation][spaces][binary number][spaces]`
- The state s₇ is an absorbing error state.
- Any character not in the alphabet is considered 'other' and transitions to the error state s₇.
- other = {c ∈ UTF-8 | c ∉ {0, 1, space, &, |, ^, EOL}} - all UTF-8 characters not included in the alphabet Σ.

## Installation and Execution

To run the project, you need to install the Stack package manager for Haskell. Then, follow these steps:

```bash
git clone https://github.com/StanleyStanMarsh/binary-parser.git
cd binary-parser
stack build
stack exec binary-parser-exe
```

When the program runs, it will prompt you for a filename to process. Enter the filename and press Enter. The program will parse the file's content and output the computation results to the console.

## Project Structure

The project contains the following components:

- `app/Main.hs`: The entry point of the application, managing user interaction and invoking core logic from the library.
- `src/Lib.hs`: A library containing pure functions and helper methods for parsing, bitwise operations, and result computation.
- `LICENSE`: The license file specifying the BSD-3-Clause License.
- `automata_bin_parser.png`: A visual representation of the finite automaton used for parsing.

## Input Data Examples

The input `.txt` file can contain lines in the following format:

- `01010 & 10101`
- `11011 ^ 00100`

When processed, the program evaluates these operations and produces the corresponding results. For instance:
- `01010 & 10101` would result in `00000`.
- `11011 ^ 00100` would result in `11111`.

## License

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

This project is licensed under the BSD-3-Clause License. You can find the complete license details in the [LICENSE](LICENSE) file.

# RU - Binary Parser

Этот проект представляет собой парсер, который читает строки из текстового файла .txt, содержащие значения и бинарные операции над ними. Поддерживаемые операции включают AND (&), OR (|) и XOR (^). Проект написан на языке Haskell и использует библиотеку Lib.hs для чистых функций и вспомогательных операций.

## Формальное описание парсера

Парсер реализован как детерминированный конечный автомат (ДКА), который распознает строки вида `  10101   |101   `.

![Диаграмма конечного автомата](automata_bin_parser.png)

*На графе operation = {&, |, ^} - множество допустимых бинарных операций*

### Формальное определение

M = (Q, Σ, δ, q₀, F), где:

- Q = {s₀, s₁, s₂, s₃, s₄, s₅, s₆, s₇} - множество состояний автомата
- Σ = {0, 1, space, &, |, ^, other, EOL} - входной алфавит
- q₀ = s₀ - начальное состояние
- F = {s₆} - множество допускающих состояний
- δ : Q × Σ → Q - функция переходов, определенная следующей таблицей:

| Состояние | space | 0,1 | &,\|,^ | other | EOL |
|-----------|-------|-----|--------|--------|-----|
| s₀        | s₀    | s₁  | s₇     | s₇     | s₇  |
| s₁        | s₂    | s₁  | s₇     | s₇     | s₇  |
| s₂        | s₂    | s₇  | s₃     | s₇     | s₇  |
| s₃        | s₃    | s₄  | s₇     | s₇     | s₇  |
| s₄        | s₅    | s₄  | s₇     | s₇     | s₇  |
| s₅        | s₅    | s₇  | s₇     | s₇     | s₆  |
| s₆        | s₇    | s₇  | s₇     | s₇     | s₇  |
| s₇        | s₇    | s₇  | s₇     | s₇     | s₇  |

### Семантика состояний:
- s₀: Начальное состояние, ожидание первого бинарного числа
- s₁: Чтение первого бинарного числа
- s₂: Ожидание бинарной операции
- s₃: Ожидание второго бинарного числа
- s₄: Чтение второго бинарного числа
- s₅: Ожидание конца строки
- s₆: Допускающее состояние
- s₇: Состояние ошибки

### Примечания:
- Автомат принимает строки формата: `[пробелы][бинарное число][пробелы][операция][пробелы][бинарное число][пробелы]`
- Состояние s₇ является поглощающим состоянием ошибки
- Любой символ, не принадлежащий алфавиту, считается как 'other' и приводит к переходу в состояние ошибки s₇
- other = {c ∈ UTF-8 | c ∉ {0, 1, space, &, |, ^, EOL}} - все символы UTF-8, не входящие в алфавит Σ

## Установка и запуск

Для запуска проекта необходимо установить пакетный менеджер Stack для Haskell. После этого выполните следующие шаги:

```bash
git clone https://github.com/StanleyStanMarsh/binary-parser.git
cd binary-parser
stack build
stack exec binary-parser-exe
```


При запуске программа запросит у вас имя файла для обработки. Введите название файла и нажмите Enter. Программа произведет синтаксический разбор содержимого файла и выведет результат вычислений на экран.

## Структура проекта

Проект состоит из нескольких файлов:

- `app/Main.hs`: Основной модуль приложения, отвечающий за взаимодействие с пользователем и вызов функций библиотеки.
- `src/Lib.hs`: Библиотека с чистыми функциями и вспомогательными методами для работы с битами и операциями.

## Примеры входных данных

Файлы могут содержать строки следующего вида:

- 01010 & 10101

- 11011 ^ 00100


Результатом выполнения этой строки будет значение, полученное после применения всех указанных операций к соответствующим битовым строкам.

## Лицензия

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Этот проект распространяется под лицензией BSD-3-Clause. См. файл [LICENSE](LICENSE) для получения дополнительной информации.
