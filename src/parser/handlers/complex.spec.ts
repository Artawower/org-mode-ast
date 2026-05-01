import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Bold test', () => {
  it('Should parse nested text formatters', () => {
    const orgDoc = '* Hello +*world*+';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          title [0-17]
            operator [0-2] ("* ")
            text [2-8] ("Hello ")
            crossed [8-17]
              operator [8-9] ("+")
              bold [9-16]
                operator [9-10] ("*")
                text [10-15] ("world")
                operator [15-16] ("*")
              operator [16-17] ("+")
      "
    `);
  });

  it('Should parse multiple opened brackets with text as simple text', () => {
    const orgDoc = 'Hello [<+*-_world';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        text [0-17] ("Hello [<+*-_world")
      "
    `);
  });

  it('Should parse nested formatted text', () => {
    const orgDoc = '*Hello +big /world/+*';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-21]
        bold [0-21]
          operator [0-1] ("*")
          text [1-7] ("Hello ")
          crossed [7-20]
            operator [7-8] ("+")
            text [8-12] ("big ")
            italic [12-19]
              operator [12-13] ("/")
              text [13-18] ("world")
              operator [18-19] ("/")
            operator [19-20] ("+")
          operator [20-21] ("*")
      "
    `);
  });

  it('Complex sample of real org node data', () => {
    const orgDoc = `
:PROPERTIES:
:ID: elisp
:PUBLISHED: true
:END:
#+TITLE: Elisp.
#+DESCRIPTION: (message "Язык состоящий на 30% из смайликов").
#+ID: elisp
#+AUTHOR: Darkawower
#+STARTUP: content
#+FILETAGS: :emacs:elisp:емакс:
#+ACTIVE: Yep!
#+begin_quote
Lisp — это не язык, а строительный материал. (Алан Кэй)
#+end_quote
* Elisp - расширеяемый функциональный язык для emacs
Все что я опишу ниже - плод моего изучения. Рекомендую изучать язык по ссылкам приведенным ниже. Я могу ошибаться, а также неправильно интерпритировать изученный мной материал.
Также, может показаться что я отношусь к лиспу как к неочень хорошо спроектированному языку. Это не так. Я отношусь так ко всем языкам. При этом автор понятия не имеет как можно что-то улучшить, и вообще... не стоит тратить время на его писульки.

** Ссылки
#+START_{SPOILER} Ресурсы для ознакомления >

+ [[htest-varps://bzg.fr/en/learn-emacs-lisp-in-15-minutes/][Emacs lisp за 15 минут]]
+ [[htest-varps://www.gnu.org/software/emacs/manual/][Emacs manual]]
+ [[htest-varp://www.paulgraham.com/onlisp.html][O lisp, книга про лисп, доступна в формате org mode]]
+ [[htest-varps://www.emacswiki.org/emacs/UnitTesting][Unit тесты]]
+ [[htest-varps://www.emacswiki.org/emacs/RegularExpression][Regexp]]
+ [[htest-varps://github.com/melpa/melpa/blob/master/CONTRIBUTING.org][Хорошее описание как сделать и законтрибутить свой пакет]]
+ [[htest-varps://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh][React + typescript emacs]]
+ [[htest-varps://www.reddit.com/r/emacs/comments/r6tq00/is_there_a_way_to_check_if_the_running_emacs_has/][Проверить флаги компиляции emacs]]
+ [[https://github.com/jwiegley/emacs-async][Асинхронность в elisp]]
+ [[https://github.com/doublep/eldev][Eldev - преблуда для разработки]]
+ [[https://www.reddit.com/r/emacs/comments/s0zvlh/formal_specification_and_programmatic_parser_for/][Мысли чувака с редита про org parser]]
+ [[https://habr.com/ru/company/sberbank/blog/655509/][Уникальность языка lisp (ru, habr)]]
#+CLOSE_{SPOILER}
** Пакеты в помощь для разработки
+ [[htest-varps://gitlab.com/niklaseklund/dtache][Dtache - пакет для запуска изолированных процессов]] ([[https://www.youtube.com/watch?v=if1W58SrClk][тут можно посмотреть]])
* Quick Start.
Быстрый старт для тех кто уже умеет программировать.

** Типы данных, переменные, константы
#+START_{SPOILER} Основа языка >

*** Объевление переменной
Такие переменные объявляются в глобальном скоупе (либо переопределяется в локальном. Локальный скоуп - =let=, но об этом ниже). Т.к. в лиспе нет изоляции на уровне модуля то хорошей практикой является использование префиксов. Часто префиксы сопостовимы с название пакета. Например *ivy*--sort.

#+begin_src emacs-lisp
(setq var "my-package--variable")
#+end_src
*** Defcustom
Переменные которые могут настраиваться с помощью *customize* - специального меню с ui полями ввода.
Значение для переменной =defcustom= можно выбирать из списка: =:options=. Разработчик плагина может заранее задать список возможных значений для таких перменных.
=:group= - значение которое позволяет группировать несколько переменных в группу, для более удобного редактирования.
Как я понял =defcustom= почти всегда > =setq=.

#+begin_src emacs-lisp
(defcustom my-custom-variable "hello"
  "Some description"
  :type 'string
  :group 'my-custom-group)

(message my-custom-variable)
#+end_src
#+RESULTS: 
: hello
*** Объявление локальной переменой
При любой возможности стоит выбирать локальную переменную, т.к. это изолирует функционал и сводит случайную перезапись к минимуму.

#+begin_src emacs-lisp
  (let ((my-var "I'am a local variable"))
     (message my-var))
#+end_src
#+RESULTS: 
: I’am a local variable

Существует ограничение, такую переменную нельзя периспользовать в блоке =let=. Чтобы ее можно было переиспользовать используется =let*=. Лично я используею везде =let*=.

#+begin_src emacs-lisp
(let* ((my-var "I'am a local variable")
       (my-var (concat my-var " And i can be overwrited!")))
  (message my-var))
#+end_src
#+RESULTS: 
: I’am a local variable And i can be overwrited!
Локальную переменную можно перезаписать, иногда это поволяет сократить избыточный код. =setq= в данном случае перезапишет *локальный* скоуп.

#+begin_src emacs-lisp
(let* ((name "Oleg"))
  (message name)
  (setq name "Vasya")
  (message name))
#+end_src
#+RESULTS: 
: Vasya
*** Объявление локальной переменной, выполняя операцию в случае если оно не nil
#+begin_src emacs-lisp
(when-let* ((b 4)
            (d nil))
  (message "This code never will be executed"))

(when-let* ((b 4)
            (d "He"))
  (message "But this code will be!"))
#+end_src
#+RESULTS: 
: But this code will be!
*** Работа с char
Char в лиспе обозначается знаком вопроса.
Конвертация осуществляется с помощью функции =string=, либо если это список из символов то с помощью функции =concat=

#+begin_src emacs-lisp

(let ((my-awesome-char ?Q))
              (message (string my-awesome-char ?H ?e ?e ?l ?o))
              (message (concat '(?W ?o ?r ?l ?d))))
#+end_src
#+RESULTS: 
: World
С помощью символов можно сделать repeat

#+begin_src emacs-lisp
(make-string 10 ?|)
#+end_src
#+RESULTS: 
: ||||||||||
*** Работа со строками
*Форматирование строки*

#+begin_src emacs-lisp
(message (format "Hello %s\n" "World?"))
#+end_src
#+RESULTS: 
: Hello World
*** Списки
Списки "экранируются" (на самом деле это не экранирование, т.к. все в лиспе функция это просто указатель на то что это не нужно исполнять, называется это evaluate /но это конечно же не точно/) с помощью симола '

#+begin_src emacs-lisp
(setq my-first-list '("Foo" "Baz" "Qwe"))
#+end_src
**** Получить первый элемент
#+begin_src emacs-lisp
(car my-first-list)
#+end_src
#+RESULTS: 
: Foo
**** Получить все кроме первого элемента..
#+begin_src emacs-lisp
(cdr my-first-list)
#+end_src
**** Добавить элемент в список
Push мутирует список. Не выглядит как нечто функциональное, но возможно я что-то не понял.

#+begin_src emacs-lisp
(setq my-first-list '())
(push "Lalalend" my-first-list)
(push "Hey" my-first-list)
#+end_src
| Hey | Lalalend |
Ну или так (последний аргумент t - добавить в конец)

#+begin_src emacs-lisp
(setq my-test-2-list '("qweqweqwe" "123"))
(add-to-list 'my-test-2-list "qwe" t)

(message "%s" my-test-2-list)
#+end_src
#+RESULTS: 
: (qweqweqwe 123 qwe)
**** Слияние 2 списков
#+begin_src emacs-lisp
(setq my-first-list '(?q ?b ?c))
(setq my-first-list (append my-first-list (list ?t)))
(message "%s" my-first-list)
#+end_src
#+RESULTS: 
: (113 98 99 116)
**** Map
На самом деле =mapcar= +(возможно создатель языка хотел иметь машину...).+

#+begin_src emacs-lisp
  (defun greeting (name)
    (format "Hello %s" name))
  
  (mapcar 'greeting my-first-list)
#+end_src
**** forEach
=mpcar= создает новый список, можно просто итерироваться по записям с помощь. dolist

#+begin_src emacs-lisp
(let* ((v ""))

  (dolist (p '("one" "two" "three"))
    (setq v (concat v " " p)))
  (message v))
#+end_src
#+RESULTS: 
:  one two three
**** Проверить есть ли элемент в списке
#+begin_src emacs-lisp
(member "123" '(1233 "qwe" "123"))
#+end_src
| 123 |
**** Перезаписать элемент в списке по индексу
#+begin_src emacs-lisp
(setq my-test-list '(("qwe" . 1) ("be" . 2)))
(setcdr (assoc "qwe" my-test-list) "asdlkajsdakd")
(message "%s" my-test-list)
#+end_src
#+RESULTS: 
: ((qwe . asdlkajsdakd) (be . 2))

А что если этого элемента нет в списке?
#+begin_src emacs-lisp
(setq my-test-list '(("be" . 2)))
(setcdr (assoc "qwe" my-test-list) "asdlkajsdakd")
(message "%s" my-test-list)
#+end_src

Не работает
**** Удалить элемент из списка
#+BEGIN_SRC emacs-lisp :results silent
ELISP> (setq list1 '(alpha beta gamma))
 (alpha beta gamma)
 
 ELISP> (setq list2 (delete 'beta list1))
 (alpha gamma)
 
 ELISP> (setq list3 (delete 'alpha list1))
 (gamma)
#+END_SRC


*** Ассоциативные массивы
**** Объявление
#+begin_src emacs-lisp
(setq trees '((a . 1) (b . "qwe")))
#+end_src
#+RESULTS: 
: ((a . 1) (b . qwe))

При чем точка нужна для специального типа /symbols/. Если работает с реальными значениями то можно и без нее

#+begin_src emacs-lisp
(setq another-hashmap '(("a" "First elem") ("b" "Second elem")))
#+end_src
**** Получить элемент по ключу
#+begin_src emacs-lisp
(message "%s" (assoc 'a trees))
#+end_src
Ну и конечно возвращает оно кортеж..а чтобы получить элемент нужно использовать уже известную нам функцию - =cdr=

#+begin_src emacs-lisp
(message "%s" (cdr (assoc 'a trees)))
#+end_src
#+RESULTS: 
: 1
**** Получить элемент по значению
#+begin_src emacs-lisp
(message "%s" (rassoc "qwe" trees))
#+end_src
При этом /rassoc/ работает и для строк и для чисел, а вот /rassq/ только для чисел

#+begin_src emacs-lisp
(message "%s" (rassq "qwe" trees)) ;; nil
(message "%s" (rassq 1 trees)) ;; (a . 1)
#+end_src
#+RESULTS: 
: (a . 1)
**** Копирование мапы
#+begin_src emacs-lisp
  (setq needles-per-cluster
        '((2 . ("Austrian Pine" "Red Pine"))
          (3 . ("Pitch Pine"))
          (5 . ("White Pine"))))
  (setq copy (copy-alist needles-per-cluster))
  (message "%s" copy)
#+end_src
#+RESULTS: 
: ((2 Austrian Pine Red Pine) (3 Pitch Pine) (5 White Pine))
**** Удаление всех записей по ключу
#+begin_src emacs-lisp
  (setq alist (list '(foo 1) '(bar 2) '(foo 3) '(lose 4)))
  (setq new-alist (assq-delete-all 'foo alist)) ;; Возвращает новое значение
  (message "%s" new-alist)
  (message (concat (format "alist: %s\n" alist)
                   (format "new: %s" new-alist)))
#+end_src
#+RESULTS: 
: alist: ((foo 1) (bar 2) (lose 4))
:  new: ((bar 2) (lose 4))
**** Удаление записей по значению
#+begin_src emacs-lisp
  (setq alist2 '((foo . first) (bar . second) (foo2 . third) (qwe . five)))
  (setq new-alist (rassq-delete-all 'third alist2)) ;; меняет значение ?
  (message "%s" new-alist)
  (message (concat (format "alist: %s\n" alist2)
                   (format "new: %s" new-alist)))
  ;; (message "%s" (rassq 'foo alist2))
#+end_src
#+RESULTS: 
: alist: ((foo . first) (bar . second) (qwe . five))
: new: ((foo . first) (bar . second) (qwe . five))
*** Хешмап
[[htest-varp://ergoemacs.org/emacs/elisp_hash_table.html][Документация]]

#+begin_src emacs-lisp
  (setq my-first-map #s(
                        hash-table
                        size 10
                        test equal
                        data (
                              python-mode "spam!"
                              go-mode "booo!1 terrible pointer"
                              org-mode "amma fluffy feature ;p"
                              )))
  (puthash 'js-mode "ugly language" my-first-map)
  (message "%s" (gethash 'python-mode my-first-map))
  (message "%s" (gethash 'js-mode my-first-map))
#+end_src
#+RESULTS: 
: ugly language
*** Символ
Тип данные соотвутствующий объекту с именем. Задаются символы с помощью 1 начальной кавычки. ='amma-symbol=

#+CLOSE_{SPOILER}
** Функции
#+START_{SPOILER} Читать про функции >

*** Объявление функций
Функции принято комментировать, это позволяет смотреть документацию в автодополнении.
Вызов =(interactive)= означается что функция публичная и может быть взывана пользователем напрямую, либо через сочетание клавиш.

#+begin_src emacs-lisp
  (defun hello (my-name)
    "This function will say hello for MY-NAME."
    (interactive)
    (message (concat "Hello, I'am " my-name)))

  (hello "Artur")
#+end_src
#+RESULTS: 
: Hello, I’am Artur
*** Опицональные аргументы
#+begin_src emacs-lisp
(defun my-super-optional-function (name &optional last-name patronymic)
  (message "%s %s %s" name (or last-name "") (or patronymic "")))

(my-super-optional-function "Artur" nil "Proshkov")
#+end_src
#+RESULTS: 
: Artur  Proshkov
*** Именованные аргументы
#+begin_src emacs-lisp
(defun my-super-function-with-named-args (&rest args)
  (message "Name %s, middle name %s" (plist-get args :name) (plist-get args :middle-name)))

  (my-super-function-with-named-args :name "One" :middle-name "Dude")
#+end_src
#+RESULTS: 
: Name One, middle name Dude
*** Лямбды
+Очевидно, лямбды нужны чтобы код можно было хуже читать+

#+begin_src emacs-lisp
(funcall '(lambda () (message "I'am dirty func")))
#+end_src
#+RESULTS: 
: I’am dirty func
*** Advice
Адвайсы это прокаченные декораторы. Могут быть вызваны как до так и после вызова оригинальной функции.

#+begin_src emacs-lisp
(defun my-increment (n)
  (+ n 1))

(defun mux-5 (n)
  (* n 5))

(advice-add 'my-increment :filter-return #'mux-5)
(message "%s" (my-increment 10))
#+end_src
#+RESULTS: 
: 55
*Пример адвайса после выполненеия функции*

#+begin_src emacs-lisp
(defun my-first-func()
  (message "qweqwe"))
(my-first-func)
(defun my-adv()
  (message "advice called"))
(advice-add :after 'my-first-func #'my-adv)
(my-first-func)
#+end_src
#+RESULTS: 
: qweqwe
*** Property list (plist)
*Установка и запись*

#+begin_src emacs-lisp
(setq my-plist '(:is-enabled t :another-prop "hey"))
(message "enabled: %s, another prop: %s, type: %s" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop) (type-of my-plist))
#+end_src
#+RESULTS: 
: enabled: t, another prop: hey, type: cons

*Изменение*

#+begin_src emacs-lisp
(setq my-plist '(:is-enabled t :another-prop "hey"))

(plist-put my-plist  :another-prop "Wow, i was changed")
(message "enabled: %s, another prop: %s" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop))
#+end_src
*Итерация по plist*

#+begin_src emacs-lisp
(setq my-plist '(:is-enabled t :another-prop "hey"))

(setq res "res: ")
(loop for (k v) on my-plist by 'cddr do
      (setq res (concat res (format "%s - %s" k v) "\n")))

;; (mapcar (lambda (k) (setq res (concat res (format "%s - " k ) "\n"))) my-plist)


;; (dolist (p my-plist)
;;   (setq res (concat res (format "%s" p) "\n")))

(message res)
#+end_src
*Удаление элемента из plist*

#+begin_src emacs-lisp
(setq test '(:hi "there" :by "man!"))

(setq test (map-delete test :hi))

(message "res: %s" test)
#+end_src
#+RESULTS: 
: res: (:by man!)

#+CLOSE_{SPOILER}
*** [[htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html][Отложенный запуск функций]]
#+begin_src emacs-lisp
(setq my-custom-timer (run-with-idle-timer 1 nil #'(lambda () (message "qwe"))))
#+end_src
#+RESULTS: 
: [nil 0 1 0 nil (lambda nil 'message "qwe") nil idle 0 nil]

Отложенные функции можно отменять

#+begin_src emacs-lisp
(cancel-timer my-custom-timer)
#+end_src
** Операторы
Орпеторы это точно такие же функции. Вынес в отдельную категорию т.к. в большинстве языков это инструкции.
#+START_{SPOILER} Детали >

*** Switch case
#+begin_src emacs-lisp
(setq test-var 'qwe)
(message "%s" (cond ((eq test-var 'q2e) 1)
       ((eq test-var 'oe) 2)
       (t "qwe")))
#+end_src
#+RESULTS: 
: qwe
*** While
#+begin_src emacs-lisp
(setq my-counter 0)
(while (< my-counter 12)
         (setq my-counter (+ my-counter 1)))

(message "%s" my-counter)
#+end_src
#+RESULTS: 
: 12
*** Catch
Просто вау, в фп есть try catch! Я действительно удивлен..даже в объектно ориетированых языках это вызывает проблемы..тем не менее..это 1 из вариантов прерываия цикла while (плохихи вариатов, как по мне, но все же)

#+begin_src emacs-lisp
(setq my-counter 0)


(message "What is the messafe from catch? Oh this is message: %s" (catch 'result
  (while (< my-counter 22)
    (setq my-counter (+ my-counter 1))
    (if (> my-counter 5)
        (throw 'result "Amma result from catch block"))
    )))
#+end_src
*** Return
Работает в *emacs 27.1+*. Позволяет прервать выполнение функции.

#+begin_src emacs-lisp
(setq my-counter 0)
(cl-defun my-iterator()
  (while (< my-counter 12)
    (if (> my-counter 3)
        (return-from my-iterator)
      )
    (setq my-counter (+ my-counter 1)))
  )

(my-iterator)

(message "%s" my-counter)
#+end_src
#+RESULTS: 
: 4

#+CLOSE_{SPOILER}
** Взаимодействие с emacs
#+START_{SPOILER} Детали >

*** Вставка в текста
#+begin_src emacs-lisp
(insert "Hello" " " "World")
#+end_src
*** Работа с буфером
**** Программное создание нового буфера
#+begin_src emacs-lisp
  (switch-to-buffer-other-window "*my-first-buffer*")
  (insert "Congratulations! I'am a new buffer")
#+end_src
**** Очистка буфера
#+begin_src emacs-lisp
(erase-buffer)
#+end_src
**** Интерактивный ввод
#+begin_src emacs-lisp
  ;; (read-from-minibuffer "Enter your name: ")
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
      (switch-to-buffer-other-window "*Your personal info")
  (erase-buffer)
  (insert (format "Hello %s!" your-name))
  (other-window 1))
#+end_src
#+RESULTS:
*** Replace в буфере
#+begin_src emacs-lisp
  (defun detect-bad-boys ()
    (setq lesson-list '("Buzova" "Volodin" "Pupin"))
  
    (defun mark-as-bad (name)
      (insert (format "Bad boy %s \n" name)))
  
    (switch-to-buffer-other-window "*lisp lesson*")
    (mapcar 'mark-as-bad lesson-list)
    (goto-char (point-min))
    (while (search-forward "Bad")
      (replace-match "Awful"))
    (other-window 1)
    )
  (detect-bad-boys)
#+end_src
*goto-char* - переход к конкретному символу
*point-min* - начало буфера
*** Добавление свойств для текста
/Перед этим необходимо запустить предыдущую функцию/

#+begin_src emacs-lisp
  ;; (detect-bad-boys)
  
  
  (defun boldify-bad-boys ()
    (switch-to-buffer-other-window "*lisp lesson*")
    (goto-char (point-min))
    (while (re-search-forward "Awful boy \\(.+\\)" nil t)
      (message (format "Its %s" (match-beginning 1)))
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           (list 'face 'bold-italic)))
    ;; (other-window 1)
    )
  
  (boldify-bad-boys)
#+end_src
#+RESULTS: 
Про сумасшедшие регекспы

#+begin_quote
;; The regular expression is "Bonjour \\(.+\\)!" and it reads:
;; the string "Bonjour ", and
;; a group of           | this is the \\( ... \\) construct
;;   any character      | this is the .
;;   possibly repeated  | this is the +
;; and the "!" string.
#+end_quote
*** Создание кнопочки
Даннный метод создает кнопку над текстом с позиции от 1 до 10.

#+begin_src emacs-lisp
(defun butest-varon-pressed (button)
  (message (format "Butest-varon pressed!")))

(define-butest-varon-type 'custom-button
  'action 'butest-varon-pressed
  'follow-link t
  'help-echo "Click Butest-varon"
  'help-args "test")

(make-butest-varon 1 10 :type 'custom-button)
#+end_src
Данная функция вставляет кнопку под текущей позицей каретки.

#+begin_src emacs-lisp
(insert-butest-varon "Press me"
               'action (lambda (_arg) (print "You are press the butest-varon!")))
#+end_src
#+RESULTS: 
: #<overlay from 1 to 10 in elisp.org>
*** Чтение из completion
#+begin_src emacs-lisp
(completing-read "Choose one: " '("foo" "bar" "baz"))
#+end_src
*** Пользовательский ввод
#+begin_src emacs-lisp
(message "U say: %s" (read-string "Say me something: "))
#+end_src
#+RESULTS: 
: U say: Ну че тут скажешь
*** Работа с выделенным текстом
**** Проверка что что-то выделено
=(use-region-p)=
**** Получить выделенный текст
#+begin_src emacs-lisp
(regionp (buffer-substring start end))
#+end_src
*** Конвертация символа в строку (ну и назад)
#+begin_src emacs-lisp
(symbol-name 'something) ;; Символ в строку
(intern (symbol-name 'something)) ;; Строка в символ
#+end_src
#+RESULTS: 
: something

#+CLOSE_{SPOILER}
*** Overlay
Overlay это очень крутая тема. Он позволяет рендерить текст который не изменяет контент реального буфера. Это может быть полезно для показа подсказок, дебага, расчитанных значений.

**** Создание оверлея в конце строки
#+begin_src emacs-lisp
(setq my-first-overlay (make-overlay (line-end-position) (line-end-position)))
#+end_src
**** Курсор заходит за предел оверлея
В моем случае курсор выходил за предел оверлея. Решается весьма просто: вставляемый в оверлей текст необходимо наделить свойством ='cursor t=

#+begin_src emacs-lisp
(setq my-popup-message (propertize popup-message 'face 'blamer--face 'cursor t))
#+end_src
**** Изменение свойств overlay
#+begin_src emacs-lisp
    (overlay-put blamer--current-overlay 'after-string my-popup-message)
    (overlay-put blamer--current-overlay 'intangible t)
    (overlay-put blamer--current-overlay 'face 'bold)
    (overlay-put blamer--current-overlay 'cursor-intangible t)
#+end_src
**** Удаление существующего оверлея
#+begin_src emacs-lisp
(if my-first-overlay
        (delete-overlay my-first-overlay))
#+end_src
*** Создание своего minor-mode :WIP:
[[htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html][Документация]]
** Работа с датами
[[htest-varps://stackoverflow.com/questions/4242012/how-do-i-add-dates-in-emacs-using-emacs-lisp][Ага, любимый стаковерфлоу]]

#+begin_src emacs-lisp
(setq t3 (time-subtract (current-time) (days-to-time 2)))
#+end_src
| 24939 | 1255 | 721279 | 0 |
#+begin_src emacs-lisp

(message "%s" (/ (float-time (time-since t3)) (* 60)))
#+end_src
#+RESULTS: 
: 2940.0710639333333
** Regexp
*** Примеры
Просто кучка примеров из разработанного мной [[htest-varps://github.com/Artawower/turbo-log][пакета]]. Регекспы весьма похожи на то что представлено в других языках. Сложно лишь работать с интерполяцией строк (неочевидна работа с большим количеством слешей в исполняемом коде.)

#+begin_src emacs-lisp
(string-match "^\\([[:blank:]]\\)*\\(return\\)" "  return {
  name: 2
}")
#+end_src
#+RESULTS: 
: 0


#+begin_src emacs-lisp
(replace-regexp-in-string "[[:blank:]]*=[[:blank:]]*.+" "" "    this.myVariable = somethingElse;")
#+end_src
#+begin_src emacs-lisp
(replace-regexp-in-string "\\(const\\|let\\|public\\|protected\\|private\\|var\\)[[:blank:]]*" "" "let anotherOne = userName")
#+end_src
#+RESULTS: 
: iable = userName
*** Regexp с группировкой
#+begin_src emacs-lisp
(concat "^(?\\(?1:[^\s]+\\) [^\s]*[[:blank:]]?\(\\(?2:[^\n]+\\)"
          "\s\\(?3:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)"
          "\s\\(?4:[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")
#+end_src
#+begin_src emacs-lisp
(setq test-string "feature/VW-221")
(string-match "\\(?1:[A-Za-z0-9]+\/\\)\\(?2:VW-[0-9]+\\)" test-string)
(message "res " (match-string 1 test-string))
#+end_src
#+RESULTS: 
: res
** Стандартные хуки
[[htest-varps://runebook.dev/ru/docs/elisp/standard-hooks][Просто смотри сюда]]
** Custom modes
*** Minor mode
Для того чтобы сделать свой minor mode достаточно его объявить и описать логику включения/выключений режима

#+begin_src emacs-lisp
;;;###autoload
(define-minor-mode wakatime-ui-mode
  "Wakatime ui mode. Add time track to doom modeline.
TODO:
Add support for other modeline in future."
  :init-value nil
  :global t
  :lighter nil
  :group 'wakatime-ui
  (if wakatime-ui-mode
      (wakatime-ui--watch-time)
    (wakatime-ui--stop-watch-time)))
#+end_src
Где:

=init-value= - значение по умолчанию
=global= - должен ли быть вызван глобальный мод перед локальным?
=lighter= - определяет что отображать в modeline когда мод включен
** Window
*** Получение ширины текущего экрана
=(window-total-width)=
** Асинхронное исполнение. Process.
Создание асинхронного процесаа ([[htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html][подробнее тут)]]

#+begin_src emacs-lisp
(setq process (start-process
                    "WakatimeUI"
                    wakatime-ui--buffer
                    (wakatime-find-binary)
                    (plist-get wakatime-ui--command-args :today-time)))
#+end_src
Чтение выходных данных из процесса

#+begin_src emacs-lisp

#+end_src
** Keymaps
*** Создание своего keymap
#+begin_src elisp
(with-current-buffer "*Messages*"
  (read-only-mode -1)
  (erase-buffer))

(setq my-mode-map (make-sparse-keymap))
(define-key my-mode-map (kbd "C-c C-'") 'my-mode-cmd1)
(define-key my-mode-map (kbd "C-c C-b") 'my-mode-cmd2)
(define-key my-mode-map (kbd "C-c C-c") 'my-mode-cmd3)
(define-key my-mode-map (kbd "<mouse-1>") 'my-mode-cmd4)
;; by convention, major mode's keys should begin with the form C-c C-‹key›

;; (dolist (m my-mode-map)
;;   (message "key: %s" m))





(map-keymap '(lambda (v g)
               (message "%s: %s" v g)) my-mode-map)
#+end_src
#+RESULTS:
** Macro
:PROPERTIES:
:ID: elisp-macros
:END:
Подробнее [[htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html][тут]].

*** Простой макрос
#+begin_src emacs-lisp
(defmacro inc (var)
  (list 'setq var (list '1+ var)))

(setq test-var 10)
(message "%s" (inc test-var))
#+end_src
#+RESULTS: 
: 11
*** Изучить макрос
Macroexpand - показывает вывод макроса

#+begin_src emacs-lisp
(message "%s" (macroexpand '(inc test-var)))
#+end_src
#+RESULTS: 
: (setq test-var (1+ tt))
*** Цепочка из макросов
Macroexpand отображает только первый макрос, чтобы вызвать всю цепочку используем =macroexpand-all=

#+begin_src emacs-lisp
(defmacro inc2 (var1 var2)
    (list 'progn (list 'inc var1) (list 'inc var2)))


(message "%s" (macroexpand-all '(inc2 r s)))
#+end_src
#+RESULTS: 
: (progn (setq r (1+ r)) (setq s (1+ s)))
*** Пример с более сложными конструкциями
#+begin_src emacs-lisp
(defmacro t-becomes-nil (var)
  \`(if (eq ,var t)
       (setq ,var nil)))

(message "%s" (t-becomes-nil test-var))
#+end_src
#+RESULTS: 
: nil
*** Динамическое получение переменной
 [[https://stackoverflow.com/questions/24433035/combining-two-variables-into-one-function-name-in-macro][Подсмотрено тут]]
 Чертовая магия 😮

#+begin_src emacs-lisp
(setq my-custom-variable "Hello, amma variable")

(defmacro get-with-prefix (var-name)
  \`(symbol-value (intern (concatenate 'string "my-custom" "-" (symbol-name ',var-name)))))

(get-with-prefix variable)
#+end_src
#+RESULTS: 
: Hello, amma variable

А теперь из plist, если нет - то из глобального скоупа, это еще большая магия. Да, наверное такое не стоит использовать в реальных проектах, но как же руки чешутся 😍

#+begin_src emacs-lisp
(setq my-custom-variable "Hello, amma variable")

(setq my-plist-with-prop '(:custom-variable nil :test t))

(defmacro get-with-prefix (my-plist var-name)
  \`(or (plist-get ,my-plist (symbol-value (intern (concatenate 'string ":" (symbol-name ',var-name)))))
       (symbol-value (intern (concatenate 'string "my" "-" (symbol-name ',var-name))))))

(message "%s" (get-with-prefix my-plist-with-prop custom-variable))
#+end_src
#+RESULTS: 
: Hello, amma variable
*** Передача тела (@body) :noexport:
Пожалуй самая впечатлаяющая фича (имхо, без нее смысл в макросах бы отпал). Макрос склеивает результаты выполнения функций (подумал для org-mode самое то)

#+begin_src emacs-lisp
(setq test-var 0)
(defmacro for (var from init to final do &rest body)
  \`(let ((,var ,init))
     (while (<= ,var ,final)
       ,@body
       (setq ,var (1+ ,var)))))


(for j from 0 to 4 do
     (setq test-var (+ test-var j))
     (setq test-var (/ test-var 2)))

(message "HAVA: %s" test-var)
#+end_src
#+RESULTS: 
: HAVA: 3


**** Failed :noexport:
Пример макроса, чтобы наглядно видеть в орге какая функция что делает

#+begin_src emacs-lisp
(defmacro pretty-log (&rest body)

  (let ((res (concat (make-string 80 ?-) "\n")))
    (dolist (f body)
      (setq res (concat res (format "[%s]: %s\n" f (eval f)))))
    (message res)))

(pretty-log (+ 1 12)
            (- 44 22)
            (+ (/ 12 2) (* 33 4))
            (setq ttt 12))
#+end_src
#+RESULTS: 
: --------------------------------------------------------------------------------
: [(+ 1 12)]: 13
: [(- 44 22)]: 22
: [(+ (/ 12 2) (* 33 4))]: 138
: [(setq ttt 12)]: 12
*** Модификация plist через список динамических аргументов как в use-package :noexport:
#+begin_src emacs-lisp
(setq res "")
(setq test-alist
      '((js-mode (:loggers '("hi there") :msg-format-template "Hi" :argument-divider "|"))
        (typescript-mode (:loggers '("another on", "and me") :msg-format-template "bee"))
        ))

(defmacro turbo-log-configure (&rest configs)
  (let* ((strategy (or (plist-get configs :strategy) 'replace))
         (excluded-keys '(:modes :strategy))
         (modes (plist-get configs :modes))
         current-config)

    (dolist (k excluded-keys)
      (setq configs (map-delete configs k)))

    (dolist (mode modes)
      (unless (assoc mode test-alist)
        (push \`(,mode nil) test-alist))

      (setq current-config (car (cdr-safe (assoc mode test-alist))))

      (if (eq strategy 'replace)
          (setq current-config configs)

        (loop for (k v) on configs by 'cddr do
              (if current-config
                  (plist-put current-config k v)
                (setq current-config \`(,k ,v)))))

      (message "QQQ: %s" configs)
      (if (assq mode test-alist)
          (setcdr (assq mode test-alist)
                  \`(,current-config))
        \`(push '(,mode '(,current-config)) ,test-alist))
      )))

(turbo-log-configure
 :modes (typescript-mode js2-mode js-mode)
 ;; :modes (typescript-mode j-mode)
 ;; :modes (js-mode)
 :strategy replace

 :loggers ("console.print" "console.dbg")
 :msg-format-template "\"HELLO WORLD: %s\"")

(message "-------------------------------------------------------")
(message "%s" (pp test-alist))
#+end_src
#+RESULTS: 
: ((mode nil)
:  (js-mode
:   (:loggers
:    '("hi there")
:    :msg-format-template "Hi"))
:  (typescript-mode
:   (:loggers
:    ("console.print" "console.dbg")
:    :msg-format-template "\"HELLO WORLD: %s\"")))
* Создание своего пакета
** Проверка ошибок компиляции
#+begin_src bash
emacs -Q --batch \
    --eval '(setq byte-compile-error-on-warn t)' \
    -f batch-byte-compile turbo-log.el
#+end_src
** Contribute
[[htest-varps://github.com/leotaku/elisp-check]]
** CI
[[htest-varps://github.com/a13/reverse-im.el/blob/master/.github/workflows/check.yml][Пример github actions]]
[[htest-varps://github.com/leotaku/elisp-check][Про elisp check]]
* Тесты
Тесты пишутся весьма просто. От части потому что не нужно мокать кучу зависимостей. Функция в большинстве случаев самодостаточна.

#+begin_src emacs-lisp
(ert-deftest my-first-test ()
  (should (= (+ 10 10) 20)))
#+end_src
Запуск.

#+begin_src bash
emacs -batch -l ert -l package.el -l test.el -f ert-run-tests-batch-and-exit
#+end_src
#+BEGIN_{HIDDEN}
* Статический анализ типов
[[https://github.com/emacs-elsa/Elsa][Его нет. Зато есть аннотации]]
* Временно :noexport:
#+begin_src emacs-lisp
(message "\"\\[line [0-9]+\\]\"")
#+end_src
#+begin_src emacs-lisp
(message "%s" (string-match "{\\|);?$" "public replaceNonPrintableCharacters(text: string): string {"))
#+end_src
#+RESULTS: 
: 59


#+begin_src emacs-lisp
(setq turbo-log--ecmascript-final-symbols '(?; ?)))
(while (or (not (eobp)) (member ?) '(?; ?))))
                 (setq current-char char-after))))
#+end_src
#+begin_src emacs-lisp
(setq quicktype-mode-configs '(("go" go-mode "")
                               ("ts" typescript-mode "")
                               ("js" js2-mode "")
                               ("rs" rust-mode "")
                               ("c++" c++-mode "")
                               ("javascript-prop-types" js2-mode "")
                               ("flow" flow-js2-mode "")
                               ("swift" swift-mode "")
                               ("kotlin" kotlin-mode "")
                               ("elm" elm-mode "")
                               ("ruby" ruby-mode "")
                               ("dart" dart-mode "")
                               ("py" python-mode "--python-version 3.7")
                               ("haskell" haskell-mode "")))

;; (message "%s" quicktype-mode-configs)
(message "%s" (cl-rassoc 'go-mode quicktype-mode-configs :test #'member))
;; (message "%s" (cl-rassoc "Red Pine" needles-per-cluster :test #'member))
#+end_src
#+RESULTS: 
: (go go-mode )


#+begin_src emacs-lisp
(setq needles-per-cluster
      '((2 "Austrian Pine" "Red Pine")
        (3 "Pitch Pine")
        (5 "White Pine")))

(message "%s" (cl-rassoc "Red Pine" needles-per-cluster :test #'member))
#+end_src
#+RESULTS: 
: (2 Austrian Pine Red Pine)


#+begin_src emacs-lisp
(message "%s" (string-match "\\({\\|;$\\)\\|\\(const [\\w\\[:digit]]+ = [\\d[:digit:]]+$\\)" "  const foo = 1"))
#+end_src
#+RESULTS: 
: nil
#+END_{HIDDEN}

#+begin_src emacs-lisp
(setq v (dolist (i '(1 2 3 4))
                i))
(message "%s" v)
#+end_src
#+RESULTS: 
: nil


** Check json
#+begin_src emacs-lisp
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file web-roam-configuration-file-path))
         (name-to-config (make-hash-table :test 'equal))
         (server-names '()))
    (dolist (config json)
      (message "%s" config))
  )
#+end_src
#+RESULTS:`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-32238]
          :id  elisp:
          :published  true:
        newLine [0-1]
        propertyDrawer [1-47]
          property [1-13]
            text [1-13] (":PROPERTIES:")
          newLine [13-14]
          property [14-24]
            text [14-19] (":ID: ")
            text [19-24] ("elisp")
          newLine [24-25]
          property [25-41]
            text [25-37] (":PUBLISHED: ")
            text [37-41] ("true")
          newLine [41-42]
          property [42-47]
            text [42-47] (":END:")
        newLine [47-48]
        keyword [48-63]
          text [48-57] ("#+TITLE: ")
          text [57-63] ("Elisp.")
        newLine [63-64]
        keyword [64-126]
          text [64-79] ("#+DESCRIPTION: ")
          text [79-126] ("(message \\"Язык состоящий на 30% из смайликов\\").")
        newLine [126-127]
        keyword [127-138]
          text [127-133] ("#+ID: ")
          text [133-138] ("elisp")
        newLine [138-139]
        keyword [139-159]
          text [139-149] ("#+AUTHOR: ")
          text [149-159] ("Darkawower")
        newLine [159-160]
        keyword [160-178]
          text [160-171] ("#+STARTUP: ")
          text [171-178] ("content")
        newLine [178-179]
        keyword [179-210]
          text [179-191] ("#+FILETAGS: ")
          tagList [191-210]
            operator [191-192] (":")
            text [192-197] ("emacs")
            operator [197-198] (":")
            text [198-203] ("elisp")
            operator [203-204] (":")
            text [204-209] ("емакс")
            operator [209-210] (":")
        newLine [210-211]
        keyword [211-225]
          text [211-221] ("#+ACTIVE: ")
          text [221-225] ("Yep!")
        newLine [225-226]
        quoteBlock [226-307]
          blockHeader [226-239]
            keyword [226-239]
              text [226-239] ("#+begin_quote")
          newLine [239-240]
          blockBody [240-295]
            text [240-295] ("Lisp — это не язык, а строительный материал. (Алан Кэй)")
          newLine [295-296]
          blockFooter [296-307]
            keyword [296-307]
              text [296-307] ("#+end_quote")
        newLine [307-308]
        headline [308-2216]
            :level 1:
          title [308-361]
            operator [308-310] ("* ")
            text [310-360] ("Elisp - расширеяемый функциональный язык для emacs")
            newLine [360-361]
          section [361-2216]
            text [361-537] ("Все что я опишу ниже - плод моего изучения. Рекомендую изучать язык по ссылкам приведенным ниже. Я могу ошибаться, а также неправильно интерпритировать изученный мной материал.")
            newLine [537-538]
            text [538-784] ("Также, может показаться что я отношусь к лиспу как к неочень хорошо спроектированному языку. Это не так. Я отношусь так ко всем языкам. При этом автор понятия не имеет как можно что-то улучшить, и вообще... не стоит тратить время на его писульки.")
            newLine [784-785]
            newLine [785-786]
            headline [786-2007]
                :level 2:
              title [786-796]
                operator [786-789] ("** ")
                text [789-795] ("Ссылки")
                newLine [795-796]
              section [796-2007]
                keyword [796-812]
                  text [796-804] ("#+START_")
                  text [804-812] ("{SPOILER")
                text [812-840] ("} Ресурсы для ознакомления >")
                newLine [840-841]
                newLine [841-842]
                list [842-1989]
                    :unordered:
                    :level 0:
                  listItem [842-928]
                    title [842-928]
                      operator [842-844] ("+ ")
                      link [844-927]
                          :linkType raw:
                        operator [844-845] ("[")
                        linkUrl [845-902]
                          operator [845-846] ("[")
                          text [846-901] ("htest-varps://bzg.fr/en/learn-emacs-lisp-in-15-minutes/")
                          operator [901-902] ("]")
                        linkName [902-926]
                          operator [902-903] ("[")
                          text [903-925] ("Emacs lisp за 15 минут")
                          operator [925-926] ("]")
                        operator [926-927] ("]")
                      newLine [927-928]
                  listItem [928-997]
                    title [928-997]
                      operator [928-930] ("+ ")
                      link [930-996]
                          :linkType raw:
                        operator [930-931] ("[")
                        linkUrl [931-981]
                          operator [931-932] ("[")
                          text [932-980] ("htest-varps://www.gnu.org/software/emacs/manual/")
                          operator [980-981] ("]")
                        linkName [981-995]
                          operator [981-982] ("[")
                          text [982-994] ("Emacs manual")
                          operator [994-995] ("]")
                        operator [995-996] ("]")
                      newLine [996-997]
                  listItem [997-1100]
                    title [997-1100]
                      operator [997-999] ("+ ")
                      link [999-1099]
                          :linkType raw:
                        operator [999-1000] ("[")
                        linkUrl [1000-1045]
                          operator [1000-1001] ("[")
                          text [1001-1044] ("htest-varp://www.paulgraham.com/onlisp.html")
                          operator [1044-1045] ("]")
                        linkName [1045-1098]
                          operator [1045-1046] ("[")
                          text [1046-1097] ("O lisp, книга про лисп, доступна в формате org mode")
                          operator [1097-1098] ("]")
                        operator [1098-1099] ("]")
                      newLine [1099-1100]
                  listItem [1100-1168]
                    title [1100-1168]
                      operator [1100-1102] ("+ ")
                      link [1102-1167]
                          :linkType raw:
                        operator [1102-1103] ("[")
                        linkUrl [1103-1154]
                          operator [1103-1104] ("[")
                          text [1104-1153] ("htest-varps://www.emacswiki.org/emacs/UnitTesting")
                          operator [1153-1154] ("]")
                        linkName [1154-1166]
                          operator [1154-1155] ("[")
                          text [1155-1165] ("Unit тесты")
                          operator [1165-1166] ("]")
                        operator [1166-1167] ("]")
                      newLine [1167-1168]
                  listItem [1168-1238]
                    title [1168-1238]
                      operator [1168-1170] ("+ ")
                      link [1170-1237]
                          :linkType raw:
                        operator [1170-1171] ("[")
                        linkUrl [1171-1228]
                          operator [1171-1172] ("[")
                          text [1172-1227] ("htest-varps://www.emacswiki.org/emacs/RegularExpression")
                          operator [1227-1228] ("]")
                        linkName [1228-1236]
                          operator [1228-1229] ("[")
                          text [1229-1235] ("Regexp")
                          operator [1235-1236] ("]")
                        operator [1236-1237] ("]")
                      newLine [1237-1238]
                  listItem [1238-1368]
                    title [1238-1368]
                      operator [1238-1240] ("+ ")
                      link [1240-1367]
                          :linkType raw:
                        operator [1240-1241] ("[")
                        linkUrl [1241-1308]
                          operator [1241-1242] ("[")
                          text [1242-1307] ("htest-varps://github.com/melpa/melpa/blob/master/CONTRIBUTING.org")
                          operator [1307-1308] ("]")
                        linkName [1308-1366]
                          operator [1308-1309] ("[")
                          text [1309-1365] ("Хорошее описание как сделать и законтрибутить свой пакет")
                          operator [1365-1366] ("]")
                        operator [1366-1367] ("]")
                      newLine [1367-1368]
                  listItem [1368-1472]
                    title [1368-1472]
                      operator [1368-1370] ("+ ")
                      link [1370-1471]
                          :linkType raw:
                        operator [1370-1371] ("[")
                        linkUrl [1371-1444]
                          operator [1371-1372] ("[")
                          text [1372-1443] ("htest-varps://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh")
                          operator [1443-1444] ("]")
                        linkName [1444-1470]
                          operator [1444-1445] ("[")
                          text [1445-1469] ("React + typescript emacs")
                          operator [1469-1470] ("]")
                        operator [1470-1471] ("]")
                      newLine [1471-1472]
                  listItem [1472-1615]
                    title [1472-1615]
                      operator [1472-1474] ("+ ")
                      link [1474-1614]
                          :linkType raw:
                        operator [1474-1475] ("[")
                        linkUrl [1475-1579]
                          operator [1475-1476] ("[")
                          text [1476-1578] ("htest-varps://www.reddit.com/r/emacs/comments/r6tq00/is_there_a_way_to_check_if_the_running_emacs_has/")
                          operator [1578-1579] ("]")
                        linkName [1579-1613]
                          operator [1579-1580] ("[")
                          text [1580-1612] ("Проверить флаги компиляции emacs")
                          operator [1612-1613] ("]")
                        operator [1613-1614] ("]")
                      newLine [1614-1615]
                  listItem [1615-1684]
                    title [1615-1684]
                      operator [1615-1617] ("+ ")
                      link [1617-1683]
                          :linkType network:
                        operator [1617-1618] ("[")
                        linkUrl [1618-1659]
                          operator [1618-1619] ("[")
                          text [1619-1658] ("https://github.com/jwiegley/emacs-async")
                          operator [1658-1659] ("]")
                        linkName [1659-1682]
                          operator [1659-1660] ("[")
                          text [1660-1681] ("Асинхронность в elisp")
                          operator [1681-1682] ("]")
                        operator [1682-1683] ("]")
                      newLine [1683-1684]
                  listItem [1684-1756]
                    title [1684-1756]
                      operator [1684-1686] ("+ ")
                      link [1686-1755]
                          :linkType network:
                        operator [1686-1687] ("[")
                        linkUrl [1687-1721]
                          operator [1687-1688] ("[")
                          text [1688-1720] ("https://github.com/doublep/eldev")
                          operator [1720-1721] ("]")
                        linkName [1721-1754]
                          operator [1721-1722] ("[")
                          text [1722-1753] ("Eldev - преблуда для разработки")
                          operator [1753-1754] ("]")
                        operator [1754-1755] ("]")
                      newLine [1755-1756]
                  listItem [1756-1897]
                    title [1756-1897]
                      operator [1756-1758] ("+ ")
                      link [1758-1896]
                          :linkType network:
                        operator [1758-1759] ("[")
                        linkUrl [1759-1857]
                          operator [1759-1760] ("[")
                          text [1760-1856] ("https://www.reddit.com/r/emacs/comments/s0zvlh/formal_specification_and_programmatic_parser_for/")
                          operator [1856-1857] ("]")
                        linkName [1857-1895]
                          operator [1857-1858] ("[")
                          text [1858-1894] ("Мысли чувака с редита про org parser")
                          operator [1894-1895] ("]")
                        operator [1895-1896] ("]")
                      newLine [1896-1897]
                  listItem [1897-1989]
                    title [1897-1989]
                      operator [1897-1899] ("+ ")
                      link [1899-1988]
                          :linkType network:
                        operator [1899-1900] ("[")
                        linkUrl [1900-1951]
                          operator [1900-1901] ("[")
                          text [1901-1950] ("https://habr.com/ru/company/sberbank/blog/655509/")
                          operator [1950-1951] ("]")
                        linkName [1951-1987]
                          operator [1951-1952] ("[")
                          text [1952-1986] ("Уникальность языка lisp (ru, habr)")
                          operator [1986-1987] ("]")
                        operator [1987-1988] ("]")
                      newLine [1988-1989]
                keyword [1989-2005]
                  text [1989-1997] ("#+CLOSE_")
                  text [1997-2005] ("{SPOILER")
                text [2005-2006] ("}")
                newLine [2006-2007]
            headline [2007-2216]
                :level 2:
              title [2007-2041]
                operator [2007-2010] ("** ")
                text [2010-2040] ("Пакеты в помощь для разработки")
                newLine [2040-2041]
              section [2041-2216]
                list [2041-2216]
                    :unordered:
                    :level 0:
                  listItem [2041-2216]
                    title [2041-2216]
                      operator [2041-2043] ("+ ")
                      link [2043-2143]
                          :linkType raw:
                        operator [2043-2044] ("[")
                        linkUrl [2044-2090]
                          operator [2044-2045] ("[")
                          text [2045-2089] ("htest-varps://gitlab.com/niklaseklund/dtache")
                          operator [2089-2090] ("]")
                        linkName [2090-2142]
                          operator [2090-2091] ("[")
                          text [2091-2141] ("Dtache - пакет для запуска изолированных процессов")
                          operator [2141-2142] ("]")
                        operator [2142-2143] ("]")
                      text [2143-2145] (" (")
                      link [2145-2214]
                          :linkType network:
                        operator [2145-2146] ("[")
                        linkUrl [2146-2191]
                          operator [2146-2147] ("[")
                          text [2147-2190] ("https://www.youtube.com/watch?v=if1W58SrClk")
                          operator [2190-2191] ("]")
                        linkName [2191-2213]
                          operator [2191-2192] ("[")
                          text [2192-2212] ("тут можно посмотреть")
                          operator [2212-2213] ("]")
                        operator [2213-2214] ("]")
                      text [2214-2215] (")")
                      newLine [2215-2216]
        headline [2216-28959]
            :level 1:
          title [2216-2231]
            operator [2216-2218] ("* ")
            text [2218-2230] ("Quick Start.")
            newLine [2230-2231]
          section [2231-28959]
            text [2231-2283] ("Быстрый старт для тех кто уже умеет программировать.")
            newLine [2283-2284]
            newLine [2284-2285]
            headline [2285-10792]
                :level 2:
              title [2285-2323]
                operator [2285-2288] ("** ")
                text [2288-2322] ("Типы данных, переменные, константы")
                newLine [2322-2323]
              section [2323-10792]
                keyword [2323-2339]
                  text [2323-2331] ("#+START_")
                  text [2331-2339] ("{SPOILER")
                text [2339-2355] ("} Основа языка >")
                newLine [2355-2356]
                newLine [2356-2357]
                headline [2357-2745]
                    :level 3:
                  title [2357-2383]
                    operator [2357-2361] ("*** ")
                    text [2361-2382] ("Объевление переменной")
                    newLine [2382-2383]
                  section [2383-2745]
                    text [2383-2486] ("Такие переменные объявляются в глобальном скоупе (либо переопределяется в локальном. Локальный скоуп - ")
                    verbatim [2486-2491]
                      operator [2486-2487] ("=")
                      text [2487-2490] ("let")
                      operator [2490-2491] ("=")
                    text [2491-2664] (", но об этом ниже). Т.к. в лиспе нет изоляции на уровне модуля то хорошей практикой является использование префиксов. Часто префиксы сопостовимы с название пакета. Например ")
                    bold [2664-2669]
                      operator [2664-2665] ("*")
                      text [2665-2668] ("ivy")
                      operator [2668-2669] ("*")
                    text [2669-2676] ("--sort.")
                    newLine [2676-2677]
                    newLine [2677-2678]
                    srcBlock [2678-2744]
                        :language emacs-lisp:
                      blockHeader [2678-2700]
                        keyword [2678-2700]
                            :language emacs-lisp:
                          text [2678-2690] ("#+begin_src ")
                          srcLanguage [2690-2700] ("emacs-lisp")
                      newLine [2700-2701]
                      blockBody [2701-2734]
                        text [2701-2734] ("(setq var \\"my-package--variable\\")")
                      newLine [2734-2735]
                      blockFooter [2735-2744]
                        keyword [2735-2744]
                          text [2735-2744] ("#+end_src")
                    newLine [2744-2745]
                headline [2745-3371]
                    :level 3:
                  title [2745-2759]
                    operator [2745-2749] ("*** ")
                    text [2749-2758] ("Defcustom")
                    newLine [2758-2759]
                  section [2759-3371]
                    text [2759-2808] ("Переменные которые могут настраиваться с помощью ")
                    bold [2808-2819]
                      operator [2808-2809] ("*")
                      text [2809-2818] ("customize")
                      operator [2818-2819] ("*")
                    text [2819-2858] (" - специального меню с ui полями ввода.")
                    newLine [2858-2859]
                    text [2859-2883] ("Значение для переменной ")
                    verbatim [2883-2894]
                      operator [2883-2884] ("=")
                      text [2884-2893] ("defcustom")
                      operator [2893-2894] ("=")
                    text [2894-3020] (" можно выбирать из списка: =:options=. Разработчик плагина может заранее задать список возможных значений для таких перменных.")
                    newLine [3020-3021]
                    text [3021-3137] ("=:group= - значение которое позволяет группировать несколько переменных в группу, для более удобного редактирования.")
                    newLine [3137-3138]
                    text [3138-3150] ("Как я понял ")
                    verbatim [3150-3161]
                      operator [3150-3151] ("=")
                      text [3151-3160] ("defcustom")
                      operator [3160-3161] ("=")
                    text [3161-3177] (" почти всегда > ")
                    verbatim [3177-3183]
                      operator [3177-3178] ("=")
                      text [3178-3182] ("setq")
                      operator [3182-3183] ("=")
                    text [3183-3184] (".")
                    newLine [3184-3185]
                    newLine [3185-3186]
                    srcBlock [3186-3350]
                        :language emacs-lisp:
                      blockHeader [3186-3208]
                        keyword [3186-3208]
                            :language emacs-lisp:
                          text [3186-3198] ("#+begin_src ")
                          srcLanguage [3198-3208] ("emacs-lisp")
                      newLine [3208-3209]
                      blockBody [3209-3340]
                        text [3209-3340] ("(defcustom my-custom-variable \\"hello\\"\\n  \\"Some description\\"\\n  :type 'string\\n  :group 'my-custom-group)\\n\\n(message my-custom-variable)")
                      newLine [3340-3341]
                      blockFooter [3341-3350]
                        keyword [3341-3350]
                          text [3341-3350] ("#+end_src")
                    newLine [3350-3351]
                    keyword [3351-3362]
                      text [3351-3362] ("#+RESULTS: ")
                    newLine [3362-3363]
                    fixedWidth [3363-3370]
                      operator [3363-3365] (": ")
                      text [3365-3370] ("hello")
                    newLine [3370-3371]
                headline [3371-4333]
                    :level 3:
                  title [3371-3406]
                    operator [3371-3375] ("*** ")
                    text [3375-3405] ("Объявление локальной переменой")
                    newLine [3405-3406]
                  section [3406-4333]
                    text [3406-3536] ("При любой возможности стоит выбирать локальную переменную, т.к. это изолирует функционал и сводит случайную перезапись к минимуму.")
                    newLine [3536-3537]
                    newLine [3537-3538]
                    srcBlock [3538-3635]
                        :language emacs-lisp:
                      blockHeader [3538-3560]
                        keyword [3538-3560]
                            :language emacs-lisp:
                          text [3538-3550] ("#+begin_src ")
                          srcLanguage [3550-3560] ("emacs-lisp")
                      newLine [3560-3561]
                      blockBody [3561-3625]
                        text [3561-3625] ("  (let ((my-var \\"I'am a local variable\\"))\\n     (message my-var))")
                      newLine [3625-3626]
                      blockFooter [3626-3635]
                        keyword [3626-3635]
                          text [3626-3635] ("#+end_src")
                    newLine [3635-3636]
                    keyword [3636-3647]
                      text [3636-3647] ("#+RESULTS: ")
                    newLine [3647-3648]
                    fixedWidth [3648-3671]
                      operator [3648-3650] (": ")
                      text [3650-3671] ("I’am a local variable")
                    newLine [3671-3672]
                    newLine [3672-3673]
                    text [3673-3745] ("Существует ограничение, такую переменную нельзя периспользовать в блоке ")
                    verbatim [3745-3750]
                      operator [3745-3746] ("=")
                      text [3746-3749] ("let")
                      operator [3749-3750] ("=")
                    text [3750-3802] (". Чтобы ее можно было переиспользовать используется ")
                    verbatim [3802-3808]
                      operator [3802-3803] ("=")
                      text [3803-3807] ("let*")
                      operator [3807-3808] ("=")
                    text [3808-3835] (". Лично я используею везде ")
                    verbatim [3835-3841]
                      operator [3835-3836] ("=")
                      text [3836-3840] ("let*")
                      operator [3840-3841] ("=")
                    text [3841-3842] (".")
                    newLine [3842-3843]
                    newLine [3843-3844]
                    srcBlock [3844-3997]
                        :language emacs-lisp:
                      blockHeader [3844-3866]
                        keyword [3844-3866]
                            :language emacs-lisp:
                          text [3844-3856] ("#+begin_src ")
                          srcLanguage [3856-3866] ("emacs-lisp")
                      newLine [3866-3867]
                      blockBody [3867-3987]
                        text [3867-3987] ("(let* ((my-var \\"I'am a local variable\\")\\n       (my-var (concat my-var \\" And i can be overwrited!\\")))\\n  (message my-var))")
                      newLine [3987-3988]
                      blockFooter [3988-3997]
                        keyword [3988-3997]
                          text [3988-3997] ("#+end_src")
                    newLine [3997-3998]
                    keyword [3998-4009]
                      text [3998-4009] ("#+RESULTS: ")
                    newLine [4009-4010]
                    fixedWidth [4010-4058]
                      operator [4010-4012] (": ")
                      text [4012-4058] ("I’am a local variable And i can be overwrited!")
                    newLine [4058-4059]
                    text [4059-4146] ("Локальную переменную можно перезаписать, иногда это поволяет сократить избыточный код. ")
                    verbatim [4146-4152]
                      operator [4146-4147] ("=")
                      text [4147-4151] ("setq")
                      operator [4151-4152] ("=")
                    text [4152-4181] (" в данном случае перезапишет ")
                    bold [4181-4192]
                      operator [4181-4182] ("*")
                      text [4182-4191] ("локальный")
                      operator [4191-4192] ("*")
                    text [4192-4199] (" скоуп.")
                    newLine [4199-4200]
                    newLine [4200-4201]
                    srcBlock [4201-4312]
                        :language emacs-lisp:
                      blockHeader [4201-4223]
                        keyword [4201-4223]
                            :language emacs-lisp:
                          text [4201-4213] ("#+begin_src ")
                          srcLanguage [4213-4223] ("emacs-lisp")
                      newLine [4223-4224]
                      blockBody [4224-4302]
                        text [4224-4302] ("(let* ((name \\"Oleg\\"))\\n  (message name)\\n  (setq name \\"Vasya\\")\\n  (message name))")
                      newLine [4302-4303]
                      blockFooter [4303-4312]
                        keyword [4303-4312]
                          text [4303-4312] ("#+end_src")
                    newLine [4312-4313]
                    keyword [4313-4324]
                      text [4313-4324] ("#+RESULTS: ")
                    newLine [4324-4325]
                    fixedWidth [4325-4332]
                      operator [4325-4327] (": ")
                      text [4327-4332] ("Vasya")
                    newLine [4332-4333]
                headline [4333-4649]
                    :level 3:
                  title [4333-4413]
                    operator [4333-4337] ("*** ")
                    text [4337-4412] ("Объявление локальной переменной, выполняя операцию в случае если оно не nil")
                    newLine [4412-4413]
                  section [4413-4649]
                    srcBlock [4413-4611]
                        :language emacs-lisp:
                      blockHeader [4413-4435]
                        keyword [4413-4435]
                            :language emacs-lisp:
                          text [4413-4425] ("#+begin_src ")
                          srcLanguage [4425-4435] ("emacs-lisp")
                      newLine [4435-4436]
                      blockBody [4436-4601]
                        text [4436-4601] ("(when-let* ((b 4)\\n            (d nil))\\n  (message \\"This code never will be executed\\"))\\n\\n(when-let* ((b 4)\\n            (d \\"He\\"))\\n  (message \\"But this code will be!\\"))")
                      newLine [4601-4602]
                      blockFooter [4602-4611]
                        keyword [4602-4611]
                          text [4602-4611] ("#+end_src")
                    newLine [4611-4612]
                    keyword [4612-4623]
                      text [4612-4623] ("#+RESULTS: ")
                    newLine [4623-4624]
                    fixedWidth [4624-4648]
                      operator [4624-4626] (": ")
                      text [4626-4648] ("But this code will be!")
                    newLine [4648-4649]
                headline [4649-5145]
                    :level 3:
                  title [4649-4667]
                    operator [4649-4653] ("*** ")
                    text [4653-4666] ("Работа с char")
                    newLine [4666-4667]
                  section [4667-5145]
                    text [4667-4708] ("Char в лиспе обозначается знаком вопроса.")
                    newLine [4708-4709]
                    text [4709-4754] ("Конвертация осуществляется с помощью функции ")
                    verbatim [4754-4762]
                      operator [4754-4755] ("=")
                      text [4755-4761] ("string")
                      operator [4761-4762] ("=")
                    text [4762-4818] (", либо если это список из символов то с помощью функции ")
                    verbatim [4818-4826]
                      operator [4818-4819] ("=")
                      text [4819-4825] ("concat")
                      operator [4825-4826] ("=")
                    newLine [4826-4827]
                    newLine [4827-4828]
                    srcBlock [4828-5005]
                        :language emacs-lisp:
                      blockHeader [4828-4850]
                        keyword [4828-4850]
                            :language emacs-lisp:
                          text [4828-4840] ("#+begin_src ")
                          srcLanguage [4840-4850] ("emacs-lisp")
                      newLine [4850-4851]
                      blockBody [4851-4995]
                        text [4851-4995] ("\\n(let ((my-awesome-char ?Q))\\n              (message (string my-awesome-char ?H ?e ?e ?l ?o))\\n              (message (concat '(?W ?o ?r ?l ?d))))")
                      newLine [4995-4996]
                      blockFooter [4996-5005]
                        keyword [4996-5005]
                          text [4996-5005] ("#+end_src")
                    newLine [5005-5006]
                    keyword [5006-5017]
                      text [5006-5017] ("#+RESULTS: ")
                    newLine [5017-5018]
                    fixedWidth [5018-5025]
                      operator [5018-5020] (": ")
                      text [5020-5025] ("World")
                    newLine [5025-5026]
                    text [5026-5065] ("С помощью символов можно сделать repeat")
                    newLine [5065-5066]
                    newLine [5066-5067]
                    srcBlock [5067-5119]
                        :language emacs-lisp:
                      blockHeader [5067-5089]
                        keyword [5067-5089]
                            :language emacs-lisp:
                          text [5067-5079] ("#+begin_src ")
                          srcLanguage [5079-5089] ("emacs-lisp")
                      newLine [5089-5090]
                      blockBody [5090-5109]
                        text [5090-5109] ("(make-string 10 ?|)")
                      newLine [5109-5110]
                      blockFooter [5110-5119]
                        keyword [5110-5119]
                          text [5110-5119] ("#+end_src")
                    newLine [5119-5120]
                    keyword [5120-5131]
                      text [5120-5131] ("#+RESULTS: ")
                    newLine [5131-5132]
                    fixedWidth [5132-5144]
                      operator [5132-5134] (": ")
                      text [5134-5144] ("||||||||||")
                    newLine [5144-5145]
                headline [5145-5292]
                    :level 3:
                  title [5145-5168]
                    operator [5145-5149] ("*** ")
                    text [5149-5167] ("Работа со строками")
                    newLine [5167-5168]
                  section [5168-5292]
                    bold [5168-5191]
                      operator [5168-5169] ("*")
                      text [5169-5190] ("Форматирование строки")
                      operator [5190-5191] ("*")
                    newLine [5191-5192]
                    newLine [5192-5193]
                    srcBlock [5193-5265]
                        :language emacs-lisp:
                      blockHeader [5193-5215]
                        keyword [5193-5215]
                            :language emacs-lisp:
                          text [5193-5205] ("#+begin_src ")
                          srcLanguage [5205-5215] ("emacs-lisp")
                      newLine [5215-5216]
                      blockBody [5216-5255]
                        text [5216-5255] ("(message (format \\"Hello %s\\n\\" \\"World?\\"))")
                      newLine [5255-5256]
                      blockFooter [5256-5265]
                        keyword [5256-5265]
                          text [5256-5265] ("#+end_src")
                    newLine [5265-5266]
                    keyword [5266-5277]
                      text [5266-5277] ("#+RESULTS: ")
                    newLine [5277-5278]
                    fixedWidth [5278-5291]
                      operator [5278-5280] (": ")
                      text [5280-5291] ("Hello World")
                    newLine [5291-5292]
                headline [5292-7773]
                    :level 3:
                  title [5292-5303]
                    operator [5292-5296] ("*** ")
                    text [5296-5302] ("Списки")
                    newLine [5302-5303]
                  section [5303-7773]
                    text [5303-5466] ("Списки \\"экранируются\\" (на самом деле это не экранирование, т.к. все в лиспе функция это просто указатель на то что это не нужно исполнять, называется это evaluate ")
                    italic [5466-5494]
                      operator [5466-5467] ("/")
                      text [5467-5493] ("но это конечно же не точно")
                      operator [5493-5494] ("/")
                    text [5494-5514] (") с помощью симола '")
                    newLine [5514-5515]
                    newLine [5515-5516]
                    srcBlock [5516-5590]
                        :language emacs-lisp:
                      blockHeader [5516-5538]
                        keyword [5516-5538]
                            :language emacs-lisp:
                          text [5516-5528] ("#+begin_src ")
                          srcLanguage [5528-5538] ("emacs-lisp")
                      newLine [5538-5539]
                      blockBody [5539-5580]
                        text [5539-5580] ("(setq my-first-list '(\\"Foo\\" \\"Baz\\" \\"Qwe\\"))")
                      newLine [5580-5581]
                      blockFooter [5581-5590]
                        keyword [5581-5590]
                          text [5581-5590] ("#+end_src")
                    newLine [5590-5591]
                    headline [5591-5691]
                        :level 4:
                      title [5591-5620]
                        operator [5591-5596] ("**** ")
                        text [5596-5619] ("Получить первый элемент")
                        newLine [5619-5620]
                      section [5620-5691]
                        srcBlock [5620-5672]
                            :language emacs-lisp:
                          blockHeader [5620-5642]
                            keyword [5620-5642]
                                :language emacs-lisp:
                              text [5620-5632] ("#+begin_src ")
                              srcLanguage [5632-5642] ("emacs-lisp")
                          newLine [5642-5643]
                          blockBody [5643-5662]
                            text [5643-5662] ("(car my-first-list)")
                          newLine [5662-5663]
                          blockFooter [5663-5672]
                            keyword [5663-5672]
                              text [5663-5672] ("#+end_src")
                        newLine [5672-5673]
                        keyword [5673-5684]
                          text [5673-5684] ("#+RESULTS: ")
                        newLine [5684-5685]
                        fixedWidth [5685-5690]
                          operator [5685-5687] (": ")
                          text [5687-5690] ("Foo")
                        newLine [5690-5691]
                    headline [5691-5787]
                        :level 4:
                      title [5691-5734]
                        operator [5691-5696] ("**** ")
                        text [5696-5733] ("Получить все кроме первого элемента..")
                        newLine [5733-5734]
                      section [5734-5787]
                        srcBlock [5734-5786]
                            :language emacs-lisp:
                          blockHeader [5734-5756]
                            keyword [5734-5756]
                                :language emacs-lisp:
                              text [5734-5746] ("#+begin_src ")
                              srcLanguage [5746-5756] ("emacs-lisp")
                          newLine [5756-5757]
                          blockBody [5757-5776]
                            text [5757-5776] ("(cdr my-first-list)")
                          newLine [5776-5777]
                          blockFooter [5777-5786]
                            keyword [5777-5786]
                              text [5777-5786] ("#+end_src")
                        newLine [5786-5787]
                    headline [5787-6279]
                        :level 4:
                      title [5787-5818]
                        operator [5787-5792] ("**** ")
                        text [5792-5817] ("Добавить элемент в список")
                        newLine [5817-5818]
                      section [5818-6279]
                        text [5818-5908] ("Push мутирует список. Не выглядит как нечто функциональное, но возможно я что-то не понял.")
                        newLine [5908-5909]
                        newLine [5909-5910]
                        srcBlock [5910-6026]
                            :language emacs-lisp:
                          blockHeader [5910-5932]
                            keyword [5910-5932]
                                :language emacs-lisp:
                              text [5910-5922] ("#+begin_src ")
                              srcLanguage [5922-5932] ("emacs-lisp")
                          newLine [5932-5933]
                          blockBody [5933-6016]
                            text [5933-6016] ("(setq my-first-list '())\\n(push \\"Lalalend\\" my-first-list)\\n(push \\"Hey\\" my-first-list)")
                          newLine [6016-6017]
                          blockFooter [6017-6026]
                            keyword [6017-6026]
                              text [6017-6026] ("#+end_src")
                        newLine [6026-6027]
                        table [6027-6045]
                          tableRow [6027-6045]
                            operator [6027-6028] ("|")
                            tableCell [6028-6033]
                              text [6028-6033] (" Hey ")
                            operator [6033-6034] ("|")
                            tableCell [6034-6044]
                              text [6034-6044] (" Lalalend ")
                            operator [6044-6045] ("|")
                        newLine [6045-6046]
                        text [6046-6098] ("Ну или так (последний аргумент t - добавить в конец)")
                        newLine [6098-6099]
                        newLine [6099-6100]
                        srcBlock [6100-6244]
                            :language emacs-lisp:
                          blockHeader [6100-6122]
                            keyword [6100-6122]
                                :language emacs-lisp:
                              text [6100-6112] ("#+begin_src ")
                              srcLanguage [6112-6122] ("emacs-lisp")
                          newLine [6122-6123]
                          blockBody [6123-6234]
                            text [6123-6234] ("(setq my-test-2-list '(\\"qweqweqwe\\" \\"123\\"))\\n(add-to-list 'my-test-2-list \\"qwe\\" t)\\n\\n(message \\"%s\\" my-test-2-list)")
                          newLine [6234-6235]
                          blockFooter [6235-6244]
                            keyword [6235-6244]
                              text [6235-6244] ("#+end_src")
                        newLine [6244-6245]
                        keyword [6245-6256]
                          text [6245-6256] ("#+RESULTS: ")
                        newLine [6256-6257]
                        fixedWidth [6257-6278]
                          operator [6257-6259] (": ")
                          text [6259-6278] ("(qweqweqwe 123 qwe)")
                        newLine [6278-6279]
                    headline [6279-6481]
                        :level 4:
                      title [6279-6302]
                        operator [6279-6284] ("**** ")
                        text [6284-6301] ("Слияние 2 списков")
                        newLine [6301-6302]
                      section [6302-6481]
                        srcBlock [6302-6450]
                            :language emacs-lisp:
                          blockHeader [6302-6324]
                            keyword [6302-6324]
                                :language emacs-lisp:
                              text [6302-6314] ("#+begin_src ")
                              srcLanguage [6314-6324] ("emacs-lisp")
                          newLine [6324-6325]
                          blockBody [6325-6440]
                            text [6325-6440] ("(setq my-first-list '(?q ?b ?c))\\n(setq my-first-list (append my-first-list (list ?t)))\\n(message \\"%s\\" my-first-list)")
                          newLine [6440-6441]
                          blockFooter [6441-6450]
                            keyword [6441-6450]
                              text [6441-6450] ("#+end_src")
                        newLine [6450-6451]
                        keyword [6451-6462]
                          text [6451-6462] ("#+RESULTS: ")
                        newLine [6462-6463]
                        fixedWidth [6463-6480]
                          operator [6463-6465] (": ")
                          text [6465-6480] ("(113 98 99 116)")
                        newLine [6480-6481]
                    headline [6481-6692]
                        :level 4:
                      title [6481-6490]
                        operator [6481-6486] ("**** ")
                        text [6486-6489] ("Map")
                        newLine [6489-6490]
                      section [6490-6692]
                        text [6490-6504] ("На самом деле ")
                        verbatim [6504-6512]
                          operator [6504-6505] ("=")
                          text [6505-6511] ("mapcar")
                          operator [6511-6512] ("=")
                        text [6512-6513] (" ")
                        crossed [6513-6564]
                          operator [6513-6514] ("+")
                          text [6514-6563] ("(возможно создатель языка хотел иметь машину...).")
                          operator [6563-6564] ("+")
                        newLine [6564-6565]
                        newLine [6565-6566]
                        srcBlock [6566-6691]
                            :language emacs-lisp:
                          blockHeader [6566-6588]
                            keyword [6566-6588]
                                :language emacs-lisp:
                              text [6566-6578] ("#+begin_src ")
                              srcLanguage [6578-6588] ("emacs-lisp")
                          newLine [6588-6589]
                          blockBody [6589-6681]
                            text [6589-6681] ("  (defun greeting (name)\\n    (format \\"Hello %s\\" name))\\n  \\n  (mapcar 'greeting my-first-list)")
                          newLine [6681-6682]
                          blockFooter [6682-6691]
                            keyword [6682-6691]
                              text [6682-6691] ("#+end_src")
                        newLine [6691-6692]
                    headline [6692-6952]
                        :level 4:
                      title [6692-6705]
                        operator [6692-6697] ("**** ")
                        text [6697-6704] ("forEach")
                        newLine [6704-6705]
                      section [6705-6952]
                        verbatim [6705-6712]
                          operator [6705-6706] ("=")
                          text [6706-6711] ("mpcar")
                          operator [6711-6712] ("=")
                        text [6712-6789] (" создает новый список, можно просто итерироваться по записям с помощь. dolist")
                        newLine [6789-6790]
                        newLine [6790-6791]
                        srcBlock [6791-6922]
                            :language emacs-lisp:
                          blockHeader [6791-6813]
                            keyword [6791-6813]
                                :language emacs-lisp:
                              text [6791-6803] ("#+begin_src ")
                              srcLanguage [6803-6813] ("emacs-lisp")
                          newLine [6813-6814]
                          blockBody [6814-6912]
                            text [6814-6912] ("(let* ((v \\"\\"))\\n\\n  (dolist (p '(\\"one\\" \\"two\\" \\"three\\"))\\n    (setq v (concat v \\" \\" p)))\\n  (message v))")
                          newLine [6912-6913]
                          blockFooter [6913-6922]
                            keyword [6913-6922]
                              text [6913-6922] ("#+end_src")
                        newLine [6922-6923]
                        keyword [6923-6934]
                          text [6923-6934] ("#+RESULTS: ")
                        newLine [6934-6935]
                        fixedWidth [6935-6951]
                          operator [6935-6937] (": ")
                          text [6937-6951] (" one two three")
                        newLine [6951-6952]
                    headline [6952-7068]
                        :level 4:
                      title [6952-6992]
                        operator [6952-6957] ("**** ")
                        text [6957-6991] ("Проверить есть ли элемент в списке")
                        newLine [6991-6992]
                      section [6992-7068]
                        srcBlock [6992-7059]
                            :language emacs-lisp:
                          blockHeader [6992-7014]
                            keyword [6992-7014]
                                :language emacs-lisp:
                              text [6992-7004] ("#+begin_src ")
                              srcLanguage [7004-7014] ("emacs-lisp")
                          newLine [7014-7015]
                          blockBody [7015-7049]
                            text [7015-7049] ("(member \\"123\\" '(1233 \\"qwe\\" \\"123\\"))")
                          newLine [7049-7050]
                          blockFooter [7050-7059]
                            keyword [7050-7059]
                              text [7050-7059] ("#+end_src")
                        newLine [7059-7060]
                        table [7060-7067]
                          tableRow [7060-7067]
                            operator [7060-7061] ("|")
                            tableCell [7061-7066]
                              text [7061-7066] (" 123 ")
                            operator [7066-7067] ("|")
                        newLine [7067-7068]
                    headline [7068-7518]
                        :level 4:
                      title [7068-7114]
                        operator [7068-7073] ("**** ")
                        text [7073-7113] ("Перезаписать элемент в списке по индексу")
                        newLine [7113-7114]
                      section [7114-7518]
                        srcBlock [7114-7271]
                            :language emacs-lisp:
                          blockHeader [7114-7136]
                            keyword [7114-7136]
                                :language emacs-lisp:
                              text [7114-7126] ("#+begin_src ")
                              srcLanguage [7126-7136] ("emacs-lisp")
                          newLine [7136-7137]
                          blockBody [7137-7261]
                            text [7137-7261] ("(setq my-test-list '((\\"qwe\\" . 1) (\\"be\\" . 2)))\\n(setcdr (assoc \\"qwe\\" my-test-list) \\"asdlkajsdakd\\")\\n(message \\"%s\\" my-test-list)")
                          newLine [7261-7262]
                          blockFooter [7262-7271]
                            keyword [7262-7271]
                              text [7262-7271] ("#+end_src")
                        newLine [7271-7272]
                        keyword [7272-7283]
                          text [7272-7283] ("#+RESULTS: ")
                        newLine [7283-7284]
                        fixedWidth [7284-7317]
                          operator [7284-7286] (": ")
                          text [7286-7317] ("((qwe . asdlkajsdakd) (be . 2))")
                        newLine [7317-7318]
                        newLine [7318-7319]
                        text [7319-7358] ("А что если этого элемента нет в списке?")
                        newLine [7358-7359]
                        srcBlock [7359-7504]
                            :language emacs-lisp:
                          blockHeader [7359-7381]
                            keyword [7359-7381]
                                :language emacs-lisp:
                              text [7359-7371] ("#+begin_src ")
                              srcLanguage [7371-7381] ("emacs-lisp")
                          newLine [7381-7382]
                          blockBody [7382-7494]
                            text [7382-7494] ("(setq my-test-list '((\\"be\\" . 2)))\\n(setcdr (assoc \\"qwe\\" my-test-list) \\"asdlkajsdakd\\")\\n(message \\"%s\\" my-test-list)")
                          newLine [7494-7495]
                          blockFooter [7495-7504]
                            keyword [7495-7504]
                              text [7495-7504] ("#+end_src")
                        newLine [7504-7505]
                        newLine [7505-7506]
                        text [7506-7517] ("Не работает")
                        newLine [7517-7518]
                    headline [7518-7773]
                        :level 4:
                      title [7518-7549]
                        operator [7518-7523] ("**** ")
                        text [7523-7548] ("Удалить элемент из списка")
                        newLine [7548-7549]
                      section [7549-7773]
                        srcBlock [7549-7770]
                            :language emacs-lisp:
                          blockHeader [7549-7587]
                            keyword [7549-7572]
                                :language emacs-lisp:
                              text [7549-7561] ("#+BEGIN_SRC ")
                              srcLanguage [7561-7572] ("emacs-lisp ")
                            blockProperty [7572-7587]
                              text [7572-7580] (":results")
                              text [7580-7587] (" silent")
                          newLine [7587-7588]
                          blockBody [7588-7760]
                            text [7588-7760] ("ELISP> (setq list1 '(alpha beta gamma))\\n (alpha beta gamma)\\n \\n ELISP> (setq list2 (delete 'beta list1))\\n (alpha gamma)\\n \\n ELISP> (setq list3 (delete 'alpha list1))\\n (gamma)")
                          newLine [7760-7761]
                          blockFooter [7761-7770]
                            keyword [7761-7770]
                              text [7761-7770] ("#+END_SRC")
                        newLine [7770-7771]
                        newLine [7771-7772]
                        newLine [7772-7773]
                headline [7773-9986]
                    :level 3:
                  title [7773-7799]
                    operator [7773-7777] ("*** ")
                    text [7777-7798] ("Ассоциативные массивы")
                    newLine [7798-7799]
                  section [7799-9986]
                    headline [7799-8127]
                        :level 4:
                      title [7799-7815]
                        operator [7799-7804] ("**** ")
                        text [7804-7814] ("Объявление")
                        newLine [7814-7815]
                      section [7815-8127]
                        srcBlock [7815-7883]
                            :language emacs-lisp:
                          blockHeader [7815-7837]
                            keyword [7815-7837]
                                :language emacs-lisp:
                              text [7815-7827] ("#+begin_src ")
                              srcLanguage [7827-7837] ("emacs-lisp")
                          newLine [7837-7838]
                          blockBody [7838-7874]
                            text [7838-7874] ("(setq trees '((a . 1) (b . \\"qwe\\")))\\n")
                          blockFooter [7874-7883]
                            keyword [7874-7883]
                              text [7874-7883] ("#+end_src")
                        newLine [7883-7884]
                        keyword [7884-7895]
                          text [7884-7895] ("#+RESULTS: ")
                        newLine [7895-7896]
                        fixedWidth [7896-7917]
                          operator [7896-7898] (": ")
                          text [7898-7917] ("((a . 1) (b . qwe))")
                        newLine [7917-7918]
                        newLine [7918-7919]
                        text [7919-7961] ("При чем точка нужна для специального типа ")
                        italic [7961-7970]
                          operator [7961-7962] ("/")
                          text [7962-7969] ("symbols")
                          operator [7969-7970] ("/")
                        text [7970-8027] (". Если работает с реальными значениями то можно и без нее")
                        newLine [8027-8028]
                        newLine [8028-8029]
                        srcBlock [8029-8126]
                            :language emacs-lisp:
                          blockHeader [8029-8051]
                            keyword [8029-8051]
                                :language emacs-lisp:
                              text [8029-8041] ("#+begin_src ")
                              srcLanguage [8041-8051] ("emacs-lisp")
                          newLine [8051-8052]
                          blockBody [8052-8116]
                            text [8052-8116] ("(setq another-hashmap '((\\"a\\" \\"First elem\\") (\\"b\\" \\"Second elem\\")))")
                          newLine [8116-8117]
                          blockFooter [8117-8126]
                            keyword [8117-8126]
                              text [8117-8126] ("#+end_src")
                        newLine [8126-8127]
                    headline [8127-8425]
                        :level 4:
                      title [8127-8158]
                        operator [8127-8132] ("**** ")
                        text [8132-8157] ("Получить элемент по ключу")
                        newLine [8157-8158]
                      section [8158-8425]
                        srcBlock [8158-8222]
                            :language emacs-lisp:
                          blockHeader [8158-8180]
                            keyword [8158-8180]
                                :language emacs-lisp:
                              text [8158-8170] ("#+begin_src ")
                              srcLanguage [8170-8180] ("emacs-lisp")
                          newLine [8180-8181]
                          blockBody [8181-8212]
                            text [8181-8212] ("(message \\"%s\\" (assoc 'a trees))")
                          newLine [8212-8213]
                          blockFooter [8213-8222]
                            keyword [8213-8222]
                              text [8213-8222] ("#+end_src")
                        newLine [8222-8223]
                        text [8223-8331] ("Ну и конечно возвращает оно кортеж..а чтобы получить элемент нужно использовать уже известную нам функцию - ")
                        verbatim [8331-8336]
                          operator [8331-8332] ("=")
                          text [8332-8335] ("cdr")
                          operator [8335-8336] ("=")
                        newLine [8336-8337]
                        newLine [8337-8338]
                        srcBlock [8338-8408]
                            :language emacs-lisp:
                          blockHeader [8338-8360]
                            keyword [8338-8360]
                                :language emacs-lisp:
                              text [8338-8350] ("#+begin_src ")
                              srcLanguage [8350-8360] ("emacs-lisp")
                          newLine [8360-8361]
                          blockBody [8361-8398]
                            text [8361-8398] ("(message \\"%s\\" (cdr (assoc 'a trees)))")
                          newLine [8398-8399]
                          blockFooter [8399-8408]
                            keyword [8399-8408]
                              text [8399-8408] ("#+end_src")
                        newLine [8408-8409]
                        keyword [8409-8420]
                          text [8409-8420] ("#+RESULTS: ")
                        newLine [8420-8421]
                        fixedWidth [8421-8424]
                          operator [8421-8423] (": ")
                          text [8423-8424] ("1")
                        newLine [8424-8425]
                    headline [8425-8751]
                        :level 4:
                      title [8425-8459]
                        operator [8425-8430] ("**** ")
                        text [8430-8458] ("Получить элемент по значению")
                        newLine [8458-8459]
                      section [8459-8751]
                        srcBlock [8459-8527]
                            :language emacs-lisp:
                          blockHeader [8459-8481]
                            keyword [8459-8481]
                                :language emacs-lisp:
                              text [8459-8471] ("#+begin_src ")
                              srcLanguage [8471-8481] ("emacs-lisp")
                          newLine [8481-8482]
                          blockBody [8482-8517]
                            text [8482-8517] ("(message \\"%s\\" (rassoc \\"qwe\\" trees))")
                          newLine [8517-8518]
                          blockFooter [8518-8527]
                            keyword [8518-8527]
                              text [8518-8527] ("#+end_src")
                        newLine [8527-8528]
                        text [8528-8537] ("При этом ")
                        italic [8537-8545]
                          operator [8537-8538] ("/")
                          text [8538-8544] ("rassoc")
                          operator [8544-8545] ("/")
                        text [8545-8586] (" работает и для строк и для чисел, а вот ")
                        italic [8586-8593]
                          operator [8586-8587] ("/")
                          text [8587-8592] ("rassq")
                          operator [8592-8593] ("/")
                        text [8593-8610] (" только для чисел")
                        newLine [8610-8611]
                        newLine [8611-8612]
                        srcBlock [8612-8728]
                            :language emacs-lisp:
                          blockHeader [8612-8634]
                            keyword [8612-8634]
                                :language emacs-lisp:
                              text [8612-8624] ("#+begin_src ")
                              srcLanguage [8624-8634] ("emacs-lisp")
                          newLine [8634-8635]
                          blockBody [8635-8718]
                            text [8635-8718] ("(message \\"%s\\" (rassq \\"qwe\\" trees)) ;; nil\\n(message \\"%s\\" (rassq 1 trees)) ;; (a . 1)")
                          newLine [8718-8719]
                          blockFooter [8719-8728]
                            keyword [8719-8728]
                              text [8719-8728] ("#+end_src")
                        newLine [8728-8729]
                        keyword [8729-8740]
                          text [8729-8740] ("#+RESULTS: ")
                        newLine [8740-8741]
                        fixedWidth [8741-8750]
                          operator [8741-8743] (": ")
                          text [8743-8750] ("(a . 1)")
                        newLine [8750-8751]
                    headline [8751-9085]
                        :level 4:
                      title [8751-8773]
                        operator [8751-8756] ("**** ")
                        text [8756-8772] ("Копирование мапы")
                        newLine [8772-8773]
                      section [8773-9085]
                        srcBlock [8773-9011]
                            :language emacs-lisp:
                          blockHeader [8773-8795]
                            keyword [8773-8795]
                                :language emacs-lisp:
                              text [8773-8785] ("#+begin_src ")
                              srcLanguage [8785-8795] ("emacs-lisp")
                          newLine [8795-8796]
                          blockBody [8796-9001]
                            text [8796-9001] ("  (setq needles-per-cluster\\n        '((2 . (\\"Austrian Pine\\" \\"Red Pine\\"))\\n          (3 . (\\"Pitch Pine\\"))\\n          (5 . (\\"White Pine\\"))))\\n  (setq copy (copy-alist needles-per-cluster))\\n  (message \\"%s\\" copy)")
                          newLine [9001-9002]
                          blockFooter [9002-9011]
                            keyword [9002-9011]
                              text [9002-9011] ("#+end_src")
                        newLine [9011-9012]
                        keyword [9012-9023]
                          text [9012-9023] ("#+RESULTS: ")
                        newLine [9023-9024]
                        fixedWidth [9024-9084]
                          operator [9024-9026] (": ")
                          text [9026-9084] ("((2 Austrian Pine Red Pine) (3 Pitch Pine) (5 White Pine))")
                        newLine [9084-9085]
                    headline [9085-9489]
                        :level 4:
                      title [9085-9121]
                        operator [9085-9090] ("**** ")
                        text [9090-9120] ("Удаление всех записей по ключу")
                        newLine [9120-9121]
                      section [9121-9489]
                        srcBlock [9121-9413]
                            :language emacs-lisp:
                          blockHeader [9121-9143]
                            keyword [9121-9143]
                                :language emacs-lisp:
                              text [9121-9133] ("#+begin_src ")
                              srcLanguage [9133-9143] ("emacs-lisp")
                          newLine [9143-9144]
                          blockBody [9144-9403]
                            text [9144-9403] ("  (setq alist (list '(foo 1) '(bar 2) '(foo 3) '(lose 4)))\\n  (setq new-alist (assq-delete-all 'foo alist)) ;; Возвращает новое значение\\n  (message \\"%s\\" new-alist)\\n  (message (concat (format \\"alist: %s\\n\\" alist)\\n                   (format \\"new: %s\\" new-alist)))")
                          newLine [9403-9404]
                          blockFooter [9404-9413]
                            keyword [9404-9413]
                              text [9404-9413] ("#+end_src")
                        newLine [9413-9414]
                        keyword [9414-9425]
                          text [9414-9425] ("#+RESULTS: ")
                        newLine [9425-9426]
                        fixedWidth [9426-9461]
                          operator [9426-9428] (": ")
                          text [9428-9461] ("alist: ((foo 1) (bar 2) (lose 4))")
                        newLine [9461-9462]
                        fixedWidth [9462-9488]
                          operator [9462-9464] (": ")
                          text [9464-9488] (" new: ((bar 2) (lose 4))")
                        newLine [9488-9489]
                    headline [9489-9986]
                        :level 4:
                      title [9489-9523]
                        operator [9489-9494] ("**** ")
                        text [9494-9522] ("Удаление записей по значению")
                        newLine [9522-9523]
                      section [9523-9986]
                        srcBlock [9523-9869]
                            :language emacs-lisp:
                          blockHeader [9523-9545]
                            keyword [9523-9545]
                                :language emacs-lisp:
                              text [9523-9535] ("#+begin_src ")
                              srcLanguage [9535-9545] ("emacs-lisp")
                          newLine [9545-9546]
                          blockBody [9546-9859]
                            text [9546-9859] ("  (setq alist2 '((foo . first) (bar . second) (foo2 . third) (qwe . five)))\\n  (setq new-alist (rassq-delete-all 'third alist2)) ;; меняет значение ?\\n  (message \\"%s\\" new-alist)\\n  (message (concat (format \\"alist: %s\\n\\" alist2)\\n                   (format \\"new: %s\\" new-alist)))\\n  ;; (message \\"%s\\" (rassq 'foo alist2))")
                          newLine [9859-9860]
                          blockFooter [9860-9869]
                            keyword [9860-9869]
                              text [9860-9869] ("#+end_src")
                        newLine [9869-9870]
                        keyword [9870-9881]
                          text [9870-9881] ("#+RESULTS: ")
                        newLine [9881-9882]
                        fixedWidth [9882-9934]
                          operator [9882-9884] (": ")
                          text [9884-9934] ("alist: ((foo . first) (bar . second) (qwe . five))")
                        newLine [9934-9935]
                        fixedWidth [9935-9985]
                          operator [9935-9937] (": ")
                          text [9937-9985] ("new: ((foo . first) (bar . second) (qwe . five))")
                        newLine [9985-9986]
                headline [9986-10654]
                    :level 3:
                  title [9986-9997]
                    operator [9986-9990] ("*** ")
                    text [9990-9996] ("Хешмап")
                    newLine [9996-9997]
                  section [9997-10654]
                    link [9997-10069]
                        :linkType raw:
                      operator [9997-9998] ("[")
                      linkUrl [9998-10054]
                        operator [9998-9999] ("[")
                        text [9999-10053] ("htest-varp://ergoemacs.org/emacs/elisp_hash_table.html")
                        operator [10053-10054] ("]")
                      linkName [10054-10068]
                        operator [10054-10055] ("[")
                        text [10055-10067] ("Документация")
                        operator [10067-10068] ("]")
                      operator [10068-10069] ("]")
                    newLine [10069-10070]
                    newLine [10070-10071]
                    srcBlock [10071-10625]
                        :language emacs-lisp:
                      blockHeader [10071-10093]
                        keyword [10071-10093]
                            :language emacs-lisp:
                          text [10071-10083] ("#+begin_src ")
                          srcLanguage [10083-10093] ("emacs-lisp")
                      newLine [10093-10094]
                      blockBody [10094-10615]
                        text [10094-10615] ("  (setq my-first-map #s(\\n                        hash-table\\n                        size 10\\n                        test equal\\n                        data (\\n                              python-mode \\"spam!\\"\\n                              go-mode \\"booo!1 terrible pointer\\"\\n                              org-mode \\"amma fluffy feature ;p\\"\\n                              )))\\n  (puthash 'js-mode \\"ugly language\\" my-first-map)\\n  (message \\"%s\\" (gethash 'python-mode my-first-map))\\n  (message \\"%s\\" (gethash 'js-mode my-first-map))")
                      newLine [10615-10616]
                      blockFooter [10616-10625]
                        keyword [10616-10625]
                          text [10616-10625] ("#+end_src")
                    newLine [10625-10626]
                    keyword [10626-10637]
                      text [10626-10637] ("#+RESULTS: ")
                    newLine [10637-10638]
                    fixedWidth [10638-10653]
                      operator [10638-10640] (": ")
                      text [10640-10653] ("ugly language")
                    newLine [10653-10654]
                headline [10654-10792]
                    :level 3:
                  title [10654-10665]
                    operator [10654-10658] ("*** ")
                    text [10658-10664] ("Символ")
                    newLine [10664-10665]
                  section [10665-10792]
                    text [10665-10758] ("Тип данные соотвутствующий объекту с именем. Задаются символы с помощью 1 начальной кавычки. ")
                    verbatim [10758-10772]
                      operator [10758-10759] ("=")
                      text [10759-10771] ("'amma-symbol")
                      operator [10771-10772] ("=")
                    newLine [10772-10773]
                    newLine [10773-10774]
                    keyword [10774-10790]
                      text [10774-10782] ("#+CLOSE_")
                      text [10782-10790] ("{SPOILER")
                    text [10790-10791] ("}")
                    newLine [10791-10792]
            headline [10792-14273]
                :level 2:
              title [10792-10803]
                operator [10792-10795] ("** ")
                text [10795-10802] ("Функции")
                newLine [10802-10803]
              section [10803-14273]
                keyword [10803-10819]
                  text [10803-10811] ("#+START_")
                  text [10811-10819] ("{SPOILER")
                text [10819-10841] ("} Читать про функции >")
                newLine [10841-10842]
                newLine [10842-10843]
                headline [10843-11304]
                    :level 3:
                  title [10843-10866]
                    operator [10843-10847] ("*** ")
                    text [10847-10865] ("Объявление функций")
                    newLine [10865-10866]
                  section [10866-11304]
                    text [10866-10951] ("Функции принято комментировать, это позволяет смотреть документацию в автодополнении.")
                    newLine [10951-10952]
                    text [10952-10958] ("Вызов ")
                    verbatim [10958-10973]
                      operator [10958-10959] ("=")
                      text [10959-10972] ("(interactive)")
                      operator [10972-10973] ("=")
                    text [10973-11080] (" означается что функция публичная и может быть взывана пользователем напрямую, либо через сочетание клавиш.")
                    newLine [11080-11081]
                    newLine [11081-11082]
                    srcBlock [11082-11271]
                        :language emacs-lisp:
                      blockHeader [11082-11104]
                        keyword [11082-11104]
                            :language emacs-lisp:
                          text [11082-11094] ("#+begin_src ")
                          srcLanguage [11094-11104] ("emacs-lisp")
                      newLine [11104-11105]
                      blockBody [11105-11261]
                        text [11105-11261] ("  (defun hello (my-name)\\n    \\"This function will say hello for MY-NAME.\\"\\n    (interactive)\\n    (message (concat \\"Hello, I'am \\" my-name)))\\n\\n  (hello \\"Artur\\")")
                      newLine [11261-11262]
                      blockFooter [11262-11271]
                        keyword [11262-11271]
                          text [11262-11271] ("#+end_src")
                    newLine [11271-11272]
                    keyword [11272-11283]
                      text [11272-11283] ("#+RESULTS: ")
                    newLine [11283-11284]
                    fixedWidth [11284-11303]
                      operator [11284-11286] (": ")
                      text [11286-11303] ("Hello, I’am Artur")
                    newLine [11303-11304]
                headline [11304-11585]
                    :level 3:
                  title [11304-11331]
                    operator [11304-11308] ("*** ")
                    text [11308-11330] ("Опицональные аргументы")
                    newLine [11330-11331]
                  section [11331-11585]
                    srcBlock [11331-11554]
                        :language emacs-lisp:
                      blockHeader [11331-11353]
                        keyword [11331-11353]
                            :language emacs-lisp:
                          text [11331-11343] ("#+begin_src ")
                          srcLanguage [11343-11353] ("emacs-lisp")
                      newLine [11353-11354]
                      blockBody [11354-11544]
                        text [11354-11544] ("(defun my-super-optional-function (name &optional last-name patronymic)\\n  (message \\"%s %s %s\\" name (or last-name \\"\\") (or patronymic \\"\\")))\\n\\n(my-super-optional-function \\"Artur\\" nil \\"Proshkov\\")")
                      newLine [11544-11545]
                      blockFooter [11545-11554]
                        keyword [11545-11554]
                          text [11545-11554] ("#+end_src")
                    newLine [11554-11555]
                    keyword [11555-11566]
                      text [11555-11566] ("#+RESULTS: ")
                    newLine [11566-11567]
                    fixedWidth [11567-11584]
                      operator [11567-11569] (": ")
                      text [11569-11584] ("Artur  Proshkov")
                    newLine [11584-11585]
                headline [11585-11902]
                    :level 3:
                  title [11585-11611]
                    operator [11585-11589] ("*** ")
                    text [11589-11610] ("Именованные аргументы")
                    newLine [11610-11611]
                  section [11611-11902]
                    srcBlock [11611-11860]
                        :language emacs-lisp:
                      blockHeader [11611-11633]
                        keyword [11611-11633]
                            :language emacs-lisp:
                          text [11611-11623] ("#+begin_src ")
                          srcLanguage [11623-11633] ("emacs-lisp")
                      newLine [11633-11634]
                      blockBody [11634-11850]
                        text [11634-11850] ("(defun my-super-function-with-named-args (&rest args)\\n  (message \\"Name %s, middle name %s\\" (plist-get args :name) (plist-get args :middle-name)))\\n\\n  (my-super-function-with-named-args :name \\"One\\" :middle-name \\"Dude\\")")
                      newLine [11850-11851]
                      blockFooter [11851-11860]
                        keyword [11851-11860]
                          text [11851-11860] ("#+end_src")
                    newLine [11860-11861]
                    keyword [11861-11872]
                      text [11861-11872] ("#+RESULTS: ")
                    newLine [11872-11873]
                    fixedWidth [11873-11901]
                      operator [11873-11875] (": ")
                      text [11875-11901] ("Name One, middle name Dude")
                    newLine [11901-11902]
                headline [11902-12086]
                    :level 3:
                  title [11902-11913]
                    operator [11902-11906] ("*** ")
                    text [11906-11912] ("Лямбды")
                    newLine [11912-11913]
                  section [11913-12086]
                    crossed [11913-11970]
                      operator [11913-11914] ("+")
                      text [11914-11969] ("Очевидно, лямбды нужны чтобы код можно было хуже читать")
                      operator [11969-11970] ("+")
                    newLine [11970-11971]
                    newLine [11971-11972]
                    srcBlock [11972-12055]
                        :language emacs-lisp:
                      blockHeader [11972-11994]
                        keyword [11972-11994]
                            :language emacs-lisp:
                          text [11972-11984] ("#+begin_src ")
                          srcLanguage [11984-11994] ("emacs-lisp")
                      newLine [11994-11995]
                      blockBody [11995-12045]
                        text [11995-12045] ("(funcall '(lambda () (message \\"I'am dirty func\\")))")
                      newLine [12045-12046]
                      blockFooter [12046-12055]
                        keyword [12046-12055]
                          text [12046-12055] ("#+end_src")
                    newLine [12055-12056]
                    keyword [12056-12067]
                      text [12056-12067] ("#+RESULTS: ")
                    newLine [12067-12068]
                    fixedWidth [12068-12085]
                      operator [12068-12070] (": ")
                      text [12070-12085] ("I’am dirty func")
                    newLine [12085-12086]
                headline [12086-12663]
                    :level 3:
                  title [12086-12097]
                    operator [12086-12090] ("*** ")
                    text [12090-12096] ("Advice")
                    newLine [12096-12097]
                  section [12097-12663]
                    text [12097-12199] ("Адвайсы это прокаченные декораторы. Могут быть вызваны как до так и после вызова оригинальной функции.")
                    newLine [12199-12200]
                    newLine [12200-12201]
                    srcBlock [12201-12381]
                        :language emacs-lisp:
                      blockHeader [12201-12223]
                        keyword [12201-12223]
                            :language emacs-lisp:
                          text [12201-12213] ("#+begin_src ")
                          srcLanguage [12213-12223] ("emacs-lisp")
                      newLine [12223-12224]
                      blockBody [12224-12371]
                        text [12224-12371] ("(defun my-increment (n)\\n  (+ n 1))\\n\\n(defun mux-5 (n)\\n  (* n 5))\\n\\n(advice-add 'my-increment :filter-return #'mux-5)\\n(message \\"%s\\" (my-increment 10))")
                      newLine [12371-12372]
                      blockFooter [12372-12381]
                        keyword [12372-12381]
                          text [12372-12381] ("#+end_src")
                    newLine [12381-12382]
                    keyword [12382-12393]
                      text [12382-12393] ("#+RESULTS: ")
                    newLine [12393-12394]
                    fixedWidth [12394-12398]
                      operator [12394-12396] (": ")
                      text [12396-12398] ("55")
                    newLine [12398-12399]
                    bold [12399-12441]
                      operator [12399-12400] ("*")
                      text [12400-12440] ("Пример адвайса после выполненеия функции")
                      operator [12440-12441] ("*")
                    newLine [12441-12442]
                    newLine [12442-12443]
                    srcBlock [12443-12641]
                        :language emacs-lisp:
                      blockHeader [12443-12465]
                        keyword [12443-12465]
                            :language emacs-lisp:
                          text [12443-12455] ("#+begin_src ")
                          srcLanguage [12455-12465] ("emacs-lisp")
                      newLine [12465-12466]
                      blockBody [12466-12631]
                        text [12466-12631] ("(defun my-first-func()\\n  (message \\"qweqwe\\"))\\n(my-first-func)\\n(defun my-adv()\\n  (message \\"advice called\\"))\\n(advice-add :after 'my-first-func #'my-adv)\\n(my-first-func)")
                      newLine [12631-12632]
                      blockFooter [12632-12641]
                        keyword [12632-12641]
                          text [12632-12641] ("#+end_src")
                    newLine [12641-12642]
                    keyword [12642-12653]
                      text [12642-12653] ("#+RESULTS: ")
                    newLine [12653-12654]
                    fixedWidth [12654-12662]
                      operator [12654-12656] (": ")
                      text [12656-12662] ("qweqwe")
                    newLine [12662-12663]
                headline [12663-13870]
                    :level 3:
                  title [12663-12689]
                    operator [12663-12667] ("*** ")
                    text [12667-12688] ("Property list (plist)")
                    newLine [12688-12689]
                  section [12689-13870]
                    bold [12689-12709]
                      operator [12689-12690] ("*")
                      text [12690-12708] ("Установка и запись")
                      operator [12708-12709] ("*")
                    newLine [12709-12710]
                    newLine [12710-12711]
                    srcBlock [12711-12935]
                        :language emacs-lisp:
                      blockHeader [12711-12733]
                        keyword [12711-12733]
                            :language emacs-lisp:
                          text [12711-12723] ("#+begin_src ")
                          srcLanguage [12723-12733] ("emacs-lisp")
                      newLine [12733-12734]
                      blockBody [12734-12925]
                        text [12734-12925] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n(message \\"enabled: %s, another prop: %s, type: %s\\" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop) (type-of my-plist))")
                      newLine [12925-12926]
                      blockFooter [12926-12935]
                        keyword [12926-12935]
                          text [12926-12935] ("#+end_src")
                    newLine [12935-12936]
                    keyword [12936-12947]
                      text [12936-12947] ("#+RESULTS: ")
                    newLine [12947-12948]
                    fixedWidth [12948-12991]
                      operator [12948-12950] (": ")
                      text [12950-12991] ("enabled: t, another prop: hey, type: cons")
                    newLine [12991-12992]
                    newLine [12992-12993]
                    bold [12993-13004]
                      operator [12993-12994] ("*")
                      text [12994-13003] ("Изменение")
                      operator [13003-13004] ("*")
                    newLine [13004-13005]
                    newLine [13005-13006]
                    srcBlock [13006-13259]
                        :language emacs-lisp:
                      blockHeader [13006-13028]
                        keyword [13006-13028]
                            :language emacs-lisp:
                          text [13006-13018] ("#+begin_src ")
                          srcLanguage [13018-13028] ("emacs-lisp")
                      newLine [13028-13029]
                      blockBody [13029-13249]
                        text [13029-13249] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n\\n(plist-put my-plist  :another-prop \\"Wow, i was changed\\")\\n(message \\"enabled: %s, another prop: %s\\" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop))")
                      newLine [13249-13250]
                      blockFooter [13250-13259]
                        keyword [13250-13259]
                          text [13250-13259] ("#+end_src")
                    newLine [13259-13260]
                    bold [13260-13279]
                      operator [13260-13261] ("*")
                      text [13261-13278] ("Итерация по plist")
                      operator [13278-13279] ("*")
                    newLine [13279-13280]
                    newLine [13280-13281]
                    srcBlock [13281-13658]
                        :language emacs-lisp:
                      blockHeader [13281-13303]
                        keyword [13281-13303]
                            :language emacs-lisp:
                          text [13281-13293] ("#+begin_src ")
                          srcLanguage [13293-13303] ("emacs-lisp")
                      newLine [13303-13304]
                      blockBody [13304-13648]
                        text [13304-13648] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n\\n(setq res \\"res: \\")\\n(loop for (k v) on my-plist by 'cddr do\\n      (setq res (concat res (format \\"%s - %s\\" k v) \\"\\n\\")))\\n\\n;; (mapcar (lambda (k) (setq res (concat res (format \\"%s - \\" k ) \\"\\n\\"))) my-plist)\\n\\n\\n;; (dolist (p my-plist)\\n;;   (setq res (concat res (format \\"%s\\" p) \\"\\n\\")))\\n\\n(message res)")
                      newLine [13648-13649]
                      blockFooter [13649-13658]
                        keyword [13649-13658]
                          text [13649-13658] ("#+end_src")
                    newLine [13658-13659]
                    bold [13659-13687]
                      operator [13659-13660] ("*")
                      text [13660-13686] ("Удаление элемента из plist")
                      operator [13686-13687] ("*")
                    newLine [13687-13688]
                    newLine [13688-13689]
                    srcBlock [13689-13820]
                        :language emacs-lisp:
                      blockHeader [13689-13711]
                        keyword [13689-13711]
                            :language emacs-lisp:
                          text [13689-13701] ("#+begin_src ")
                          srcLanguage [13701-13711] ("emacs-lisp")
                      newLine [13711-13712]
                      blockBody [13712-13810]
                        text [13712-13810] ("(setq test '(:hi \\"there\\" :by \\"man!\\"))\\n\\n(setq test (map-delete test :hi))\\n\\n(message \\"res: %s\\" test)")
                      newLine [13810-13811]
                      blockFooter [13811-13820]
                        keyword [13811-13820]
                          text [13811-13820] ("#+end_src")
                    newLine [13820-13821]
                    keyword [13821-13832]
                      text [13821-13832] ("#+RESULTS: ")
                    newLine [13832-13833]
                    fixedWidth [13833-13850]
                      operator [13833-13835] (": ")
                      text [13835-13850] ("res: (:by man!)")
                    newLine [13850-13851]
                    newLine [13851-13852]
                    keyword [13852-13868]
                      text [13852-13860] ("#+CLOSE_")
                      text [13860-13868] ("{SPOILER")
                    text [13868-13869] ("}")
                    newLine [13869-13870]
                headline [13870-14273]
                    :level 3:
                  title [13870-13986]
                    operator [13870-13874] ("*** ")
                    link [13874-13985]
                        :linkType raw:
                      operator [13874-13875] ("[")
                      linkUrl [13875-13957]
                        operator [13875-13876] ("[")
                        text [13876-13956] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html")
                        operator [13956-13957] ("]")
                      linkName [13957-13984]
                        operator [13957-13958] ("[")
                        text [13958-13983] ("Отложенный запуск функций")
                        operator [13983-13984] ("]")
                      operator [13984-13985] ("]")
                    newLine [13985-13986]
                  section [13986-14273]
                    srcBlock [13986-14099]
                        :language emacs-lisp:
                      blockHeader [13986-14008]
                        keyword [13986-14008]
                            :language emacs-lisp:
                          text [13986-13998] ("#+begin_src ")
                          srcLanguage [13998-14008] ("emacs-lisp")
                      newLine [14008-14009]
                      blockBody [14009-14089]
                        text [14009-14089] ("(setq my-custom-timer (run-with-idle-timer 1 nil #'(lambda () (message \\"qwe\\"))))")
                      newLine [14089-14090]
                      blockFooter [14090-14099]
                        keyword [14090-14099]
                          text [14090-14099] ("#+end_src")
                    newLine [14099-14100]
                    keyword [14100-14111]
                      text [14100-14111] ("#+RESULTS: ")
                    newLine [14111-14112]
                    fixedWidth [14112-14172]
                      operator [14112-14114] (": ")
                      text [14114-14172] ("[nil 0 1 0 nil (lambda nil 'message \\"qwe\\") nil idle 0 nil]")
                    newLine [14172-14173]
                    newLine [14173-14174]
                    text [14174-14207] ("Отложенные функции можно отменять")
                    newLine [14207-14208]
                    newLine [14208-14209]
                    srcBlock [14209-14272]
                        :language emacs-lisp:
                      blockHeader [14209-14231]
                        keyword [14209-14231]
                            :language emacs-lisp:
                          text [14209-14221] ("#+begin_src ")
                          srcLanguage [14221-14231] ("emacs-lisp")
                      newLine [14231-14232]
                      blockBody [14232-14262]
                        text [14232-14262] ("(cancel-timer my-custom-timer)")
                      newLine [14262-14263]
                      blockFooter [14263-14272]
                        keyword [14263-14272]
                          text [14263-14272] ("#+end_src")
                    newLine [14272-14273]
            headline [14273-15663]
                :level 2:
              title [14273-14286]
                operator [14273-14276] ("** ")
                text [14276-14285] ("Операторы")
                newLine [14285-14286]
              section [14286-15663]
                text [14286-14392] ("Орпеторы это точно такие же функции. Вынес в отдельную категорию т.к. в большинстве языков это инструкции.")
                newLine [14392-14393]
                keyword [14393-14409]
                  text [14393-14401] ("#+START_")
                  text [14401-14409] ("{SPOILER")
                text [14409-14419] ("} Детали >")
                newLine [14419-14420]
                newLine [14420-14421]
                headline [14421-14600]
                    :level 3:
                  title [14421-14437]
                    operator [14421-14425] ("*** ")
                    text [14425-14436] ("Switch case")
                    newLine [14436-14437]
                  section [14437-14600]
                    srcBlock [14437-14581]
                        :language emacs-lisp:
                      blockHeader [14437-14459]
                        keyword [14437-14459]
                            :language emacs-lisp:
                          text [14437-14449] ("#+begin_src ")
                          srcLanguage [14449-14459] ("emacs-lisp")
                      newLine [14459-14460]
                      blockBody [14460-14571]
                        text [14460-14571] ("(setq test-var 'qwe)\\n(message \\"%s\\" (cond ((eq test-var 'q2e) 1)\\n       ((eq test-var 'oe) 2)\\n       (t \\"qwe\\")))")
                      newLine [14571-14572]
                      blockFooter [14572-14581]
                        keyword [14572-14581]
                          text [14572-14581] ("#+end_src")
                    newLine [14581-14582]
                    keyword [14582-14593]
                      text [14582-14593] ("#+RESULTS: ")
                    newLine [14593-14594]
                    fixedWidth [14594-14599]
                      operator [14594-14596] (": ")
                      text [14596-14599] ("qwe")
                    newLine [14599-14600]
                headline [14600-14777]
                    :level 3:
                  title [14600-14610]
                    operator [14600-14604] ("*** ")
                    text [14604-14609] ("While")
                    newLine [14609-14610]
                  section [14610-14777]
                    srcBlock [14610-14759]
                        :language emacs-lisp:
                      blockHeader [14610-14632]
                        keyword [14610-14632]
                            :language emacs-lisp:
                          text [14610-14622] ("#+begin_src ")
                          srcLanguage [14622-14632] ("emacs-lisp")
                      newLine [14632-14633]
                      blockBody [14633-14749]
                        text [14633-14749] ("(setq my-counter 0)\\n(while (< my-counter 12)\\n         (setq my-counter (+ my-counter 1)))\\n\\n(message \\"%s\\" my-counter)")
                      newLine [14749-14750]
                      blockFooter [14750-14759]
                        keyword [14750-14759]
                          text [14750-14759] ("#+end_src")
                    newLine [14759-14760]
                    keyword [14760-14771]
                      text [14760-14771] ("#+RESULTS: ")
                    newLine [14771-14772]
                    fixedWidth [14772-14776]
                      operator [14772-14774] (": ")
                      text [14774-14776] ("12")
                    newLine [14776-14777]
                headline [14777-15294]
                    :level 3:
                  title [14777-14787]
                    operator [14777-14781] ("*** ")
                    text [14781-14786] ("Catch")
                    newLine [14786-14787]
                  section [14787-15294]
                    text [14787-15001] ("Просто вау, в фп есть try catch! Я действительно удивлен..даже в объектно ориетированых языках это вызывает проблемы..тем не менее..это 1 из вариантов прерываия цикла while (плохихи вариатов, как по мне, но все же)")
                    newLine [15001-15002]
                    newLine [15002-15003]
                    srcBlock [15003-15293]
                        :language emacs-lisp:
                      blockHeader [15003-15025]
                        keyword [15003-15025]
                            :language emacs-lisp:
                          text [15003-15015] ("#+begin_src ")
                          srcLanguage [15015-15025] ("emacs-lisp")
                      newLine [15025-15026]
                      blockBody [15026-15283]
                        text [15026-15283] ("(setq my-counter 0)\\n\\n\\n(message \\"What is the messafe from catch? Oh this is message: %s\\" (catch 'result\\n  (while (< my-counter 22)\\n    (setq my-counter (+ my-counter 1))\\n    (if (> my-counter 5)\\n        (throw 'result \\"Amma result from catch block\\"))\\n    )))")
                      newLine [15283-15284]
                      blockFooter [15284-15293]
                        keyword [15284-15293]
                          text [15284-15293] ("#+end_src")
                    newLine [15293-15294]
                headline [15294-15663]
                    :level 3:
                  title [15294-15305]
                    operator [15294-15298] ("*** ")
                    text [15298-15304] ("Return")
                    newLine [15304-15305]
                  section [15305-15663]
                    text [15305-15316] ("Работает в ")
                    bold [15316-15329]
                      operator [15316-15317] ("*")
                      text [15317-15328] ("emacs 27.1+")
                      operator [15328-15329] ("*")
                    text [15329-15369] (". Позволяет прервать выполнение функции.")
                    newLine [15369-15370]
                    newLine [15370-15371]
                    srcBlock [15371-15627]
                        :language emacs-lisp:
                      blockHeader [15371-15393]
                        keyword [15371-15393]
                            :language emacs-lisp:
                          text [15371-15383] ("#+begin_src ")
                          srcLanguage [15383-15393] ("emacs-lisp")
                      newLine [15393-15394]
                      blockBody [15394-15617]
                        text [15394-15617] ("(setq my-counter 0)\\n(cl-defun my-iterator()\\n  (while (< my-counter 12)\\n    (if (> my-counter 3)\\n        (return-from my-iterator)\\n      )\\n    (setq my-counter (+ my-counter 1)))\\n  )\\n\\n(my-iterator)\\n\\n(message \\"%s\\" my-counter)")
                      newLine [15617-15618]
                      blockFooter [15618-15627]
                        keyword [15618-15627]
                          text [15618-15627] ("#+end_src")
                    newLine [15627-15628]
                    keyword [15628-15639]
                      text [15628-15639] ("#+RESULTS: ")
                    newLine [15639-15640]
                    fixedWidth [15640-15643]
                      operator [15640-15642] (": ")
                      text [15642-15643] ("4")
                    newLine [15643-15644]
                    newLine [15644-15645]
                    keyword [15645-15661]
                      text [15645-15653] ("#+CLOSE_")
                      text [15653-15661] ("{SPOILER")
                    text [15661-15662] ("}")
                    newLine [15662-15663]
            headline [15663-20336]
                :level 2:
              title [15663-15689]
                operator [15663-15666] ("** ")
                text [15666-15688] ("Взаимодействие с emacs")
                newLine [15688-15689]
              section [15689-20336]
                keyword [15689-15705]
                  text [15689-15697] ("#+START_")
                  text [15697-15705] ("{SPOILER")
                text [15705-15715] ("} Детали >")
                newLine [15715-15716]
                newLine [15716-15717]
                headline [15717-15800]
                    :level 3:
                  title [15717-15738]
                    operator [15717-15721] ("*** ")
                    text [15721-15737] ("Вставка в текста")
                    newLine [15737-15738]
                  section [15738-15800]
                    srcBlock [15738-15799]
                        :language emacs-lisp:
                      blockHeader [15738-15760]
                        keyword [15738-15760]
                            :language emacs-lisp:
                          text [15738-15750] ("#+begin_src ")
                          srcLanguage [15750-15760] ("emacs-lisp")
                      newLine [15760-15761]
                      blockBody [15761-15789]
                        text [15761-15789] ("(insert \\"Hello\\" \\" \\" \\"World\\")")
                      newLine [15789-15790]
                      blockFooter [15790-15799]
                        keyword [15790-15799]
                          text [15790-15799] ("#+end_src")
                    newLine [15799-15800]
                headline [15800-16383]
                    :level 3:
                  title [15800-15821]
                    operator [15800-15804] ("*** ")
                    text [15804-15820] ("Работа с буфером")
                    newLine [15820-15821]
                  section [15821-16383]
                    headline [15821-15996]
                        :level 4:
                      title [15821-15861]
                        operator [15821-15826] ("**** ")
                        text [15826-15860] ("Программное создание нового буфера")
                        newLine [15860-15861]
                      section [15861-15996]
                        srcBlock [15861-15995]
                            :language emacs-lisp:
                          blockHeader [15861-15883]
                            keyword [15861-15883]
                                :language emacs-lisp:
                              text [15861-15873] ("#+begin_src ")
                              srcLanguage [15873-15883] ("emacs-lisp")
                          newLine [15883-15884]
                          blockBody [15884-15985]
                            text [15884-15985] ("  (switch-to-buffer-other-window \\"*my-first-buffer*\\")\\n  (insert \\"Congratulations! I'am a new buffer\\")")
                          newLine [15985-15986]
                          blockFooter [15986-15995]
                            keyword [15986-15995]
                              text [15986-15995] ("#+end_src")
                        newLine [15995-15996]
                    headline [15996-16064]
                        :level 4:
                      title [15996-16016]
                        operator [15996-16001] ("**** ")
                        text [16001-16015] ("Очистка буфера")
                        newLine [16015-16016]
                      section [16016-16064]
                        srcBlock [16016-16063]
                            :language emacs-lisp:
                          blockHeader [16016-16038]
                            keyword [16016-16038]
                                :language emacs-lisp:
                              text [16016-16028] ("#+begin_src ")
                              srcLanguage [16028-16038] ("emacs-lisp")
                          newLine [16038-16039]
                          blockBody [16039-16053]
                            text [16039-16053] ("(erase-buffer)")
                          newLine [16053-16054]
                          blockFooter [16054-16063]
                            keyword [16054-16063]
                              text [16054-16063] ("#+end_src")
                        newLine [16063-16064]
                    headline [16064-16383]
                        :level 4:
                      title [16064-16088]
                        operator [16064-16069] ("**** ")
                        text [16069-16087] ("Интерактивный ввод")
                        newLine [16087-16088]
                      section [16088-16383]
                        srcBlock [16088-16371]
                            :language emacs-lisp:
                          blockHeader [16088-16110]
                            keyword [16088-16110]
                                :language emacs-lisp:
                              text [16088-16100] ("#+begin_src ")
                              srcLanguage [16100-16110] ("emacs-lisp")
                          newLine [16110-16111]
                          blockBody [16111-16361]
                            text [16111-16361] ("  ;; (read-from-minibuffer \\"Enter your name: \\")\\n  (let ((your-name (read-from-minibuffer \\"Enter your name: \\")))\\n      (switch-to-buffer-other-window \\"*Your personal info\\")\\n  (erase-buffer)\\n  (insert (format \\"Hello %s!\\" your-name))\\n  (other-window 1))")
                          newLine [16361-16362]
                          blockFooter [16362-16371]
                            keyword [16362-16371]
                              text [16362-16371] ("#+end_src")
                        newLine [16371-16372]
                        keyword [16372-16382]
                          text [16372-16382] ("#+RESULTS:")
                        newLine [16382-16383]
                headline [16383-16901]
                    :level 3:
                  title [16383-16404]
                    operator [16383-16387] ("*** ")
                    text [16387-16403] ("Replace в буфере")
                    newLine [16403-16404]
                  section [16404-16901]
                    srcBlock [16404-16828]
                        :language emacs-lisp:
                      blockHeader [16404-16426]
                        keyword [16404-16426]
                            :language emacs-lisp:
                          text [16404-16416] ("#+begin_src ")
                          srcLanguage [16416-16426] ("emacs-lisp")
                      newLine [16426-16427]
                      blockBody [16427-16818]
                        text [16427-16818] ("  (defun detect-bad-boys ()\\n    (setq lesson-list '(\\"Buzova\\" \\"Volodin\\" \\"Pupin\\"))\\n  \\n    (defun mark-as-bad (name)\\n      (insert (format \\"Bad boy %s \\n\\" name)))\\n  \\n    (switch-to-buffer-other-window \\"*lisp lesson*\\")\\n    (mapcar 'mark-as-bad lesson-list)\\n    (goto-char (point-min))\\n    (while (search-forward \\"Bad\\")\\n      (replace-match \\"Awful\\"))\\n    (other-window 1)\\n    )\\n  (detect-bad-boys)")
                      newLine [16818-16819]
                      blockFooter [16819-16828]
                        keyword [16819-16828]
                          text [16819-16828] ("#+end_src")
                    newLine [16828-16829]
                    bold [16829-16840]
                      operator [16829-16830] ("*")
                      text [16830-16839] ("goto-char")
                      operator [16839-16840] ("*")
                    text [16840-16872] (" - переход к конкретному символу")
                    newLine [16872-16873]
                    bold [16873-16884]
                      operator [16873-16874] ("*")
                      text [16874-16883] ("point-min")
                      operator [16883-16884] ("*")
                    text [16884-16900] (" - начало буфера")
                    newLine [16900-16901]
                headline [16901-17783]
                    :level 3:
                  title [16901-16935]
                    operator [16901-16905] ("*** ")
                    text [16905-16934] ("Добавление свойств для текста")
                    newLine [16934-16935]
                  section [16935-17783]
                    italic [16935-16987]
                      operator [16935-16936] ("/")
                      text [16936-16986] ("Перед этим необходимо запустить предыдущую функцию")
                      operator [16986-16987] ("/")
                    newLine [16987-16988]
                    newLine [16988-16989]
                    srcBlock [16989-17466]
                        :language emacs-lisp:
                      blockHeader [16989-17011]
                        keyword [16989-17011]
                            :language emacs-lisp:
                          text [16989-17001] ("#+begin_src ")
                          srcLanguage [17001-17011] ("emacs-lisp")
                      newLine [17011-17012]
                      blockBody [17012-17456]
                        text [17012-17456] ("  ;; (detect-bad-boys)\\n  \\n  \\n  (defun boldify-bad-boys ()\\n    (switch-to-buffer-other-window \\"*lisp lesson*\\")\\n    (goto-char (point-min))\\n    (while (re-search-forward \\"Awful boy \\\\(.+\\\\)\\" nil t)\\n      (message (format \\"Its %s\\" (match-beginning 1)))\\n      (add-text-properties (match-beginning 1)\\n                           (match-end 1)\\n                           (list 'face 'bold-italic)))\\n    ;; (other-window 1)\\n    )\\n  \\n  (boldify-bad-boys)")
                      newLine [17456-17457]
                      blockFooter [17457-17466]
                        keyword [17457-17466]
                          text [17457-17466] ("#+end_src")
                    newLine [17466-17467]
                    keyword [17467-17478]
                      text [17467-17478] ("#+RESULTS: ")
                    newLine [17478-17479]
                    text [17479-17503] ("Про сумасшедшие регекспы")
                    newLine [17503-17504]
                    newLine [17504-17505]
                    quoteBlock [17505-17782]
                      blockHeader [17505-17518]
                        keyword [17505-17518]
                          text [17505-17518] ("#+begin_quote")
                      newLine [17518-17519]
                      blockBody [17519-17770]
                        text [17519-17557] (";; The regular expression is \\"Bonjour ")
                        keyword [17557-17579]
                          text [17557-17561] ("\\\\(.+")
                          text [17561-17579] ("\\\\)!\\" and it reads:")
                        newLine [17579-17580]
                        text [17580-17609] (";; the string \\"Bonjour \\", and")
                        newLine [17609-17610]
                        text [17610-17648] (";; a group of           | this is the ")
                        keyword [17648-17655]
                          text [17648-17655] ("\\\\( ... ")
                        keyword [17655-17667]
                          text [17655-17667] ("\\\\) construct")
                        newLine [17667-17668]
                        text [17668-17707] (";;   any character      | this is the .")
                        newLine [17707-17708]
                        text [17708-17747] (";;   possibly repeated  | this is the +")
                        newLine [17747-17748]
                        text [17748-17770] (";; and the \\"!\\" string.")
                      newLine [17770-17771]
                      blockFooter [17771-17782]
                        keyword [17771-17782]
                          text [17771-17782] ("#+end_quote")
                    newLine [17782-17783]
                headline [17783-18438]
                    :level 3:
                  title [17783-17805]
                    operator [17783-17787] ("*** ")
                    text [17787-17804] ("Создание кнопочки")
                    newLine [17804-17805]
                  section [17805-18438]
                    text [17805-17867] ("Даннный метод создает кнопку над текстом с позиции от 1 до 10.")
                    newLine [17867-17868]
                    newLine [17868-17869]
                    srcBlock [17869-18177]
                        :language emacs-lisp:
                      blockHeader [17869-17891]
                        keyword [17869-17891]
                            :language emacs-lisp:
                          text [17869-17881] ("#+begin_src ")
                          srcLanguage [17881-17891] ("emacs-lisp")
                      newLine [17891-17892]
                      blockBody [17892-18167]
                        text [17892-18167] ("(defun butest-varon-pressed (button)\\n  (message (format \\"Butest-varon pressed!\\")))\\n\\n(define-butest-varon-type 'custom-button\\n  'action 'butest-varon-pressed\\n  'follow-link t\\n  'help-echo \\"Click Butest-varon\\"\\n  'help-args \\"test\\")\\n\\n(make-butest-varon 1 10 :type 'custom-button)")
                      newLine [18167-18168]
                      blockFooter [18168-18177]
                        keyword [18168-18177]
                          text [18168-18177] ("#+end_src")
                    newLine [18177-18178]
                    text [18178-18238] ("Данная функция вставляет кнопку под текущей позицей каретки.")
                    newLine [18238-18239]
                    newLine [18239-18240]
                    srcBlock [18240-18386]
                        :language emacs-lisp:
                      blockHeader [18240-18262]
                        keyword [18240-18262]
                            :language emacs-lisp:
                          text [18240-18252] ("#+begin_src ")
                          srcLanguage [18252-18262] ("emacs-lisp")
                      newLine [18262-18263]
                      blockBody [18263-18376]
                        text [18263-18376] ("(insert-butest-varon \\"Press me\\"\\n               'action (lambda (_arg) (print \\"You are press the butest-varon!\\")))")
                      newLine [18376-18377]
                      blockFooter [18377-18386]
                        keyword [18377-18386]
                          text [18377-18386] ("#+end_src")
                    newLine [18386-18387]
                    keyword [18387-18398]
                      text [18387-18398] ("#+RESULTS: ")
                    newLine [18398-18399]
                    fixedWidth [18399-18437]
                      operator [18399-18401] (": ")
                      text [18401-18437] ("#<overlay from 1 to 10 in elisp.org>")
                    newLine [18437-18438]
                headline [18438-18550]
                    :level 3:
                  title [18438-18463]
                    operator [18438-18442] ("*** ")
                    text [18442-18462] ("Чтение из completion")
                    newLine [18462-18463]
                  section [18463-18550]
                    srcBlock [18463-18549]
                        :language emacs-lisp:
                      blockHeader [18463-18485]
                        keyword [18463-18485]
                            :language emacs-lisp:
                          text [18463-18475] ("#+begin_src ")
                          srcLanguage [18475-18485] ("emacs-lisp")
                      newLine [18485-18486]
                      blockBody [18486-18539]
                        text [18486-18539] ("(completing-read \\"Choose one: \\" '(\\"foo\\" \\"bar\\" \\"baz\\"))")
                      newLine [18539-18540]
                      blockFooter [18540-18549]
                        keyword [18540-18549]
                          text [18540-18549] ("#+end_src")
                    newLine [18549-18550]
                headline [18550-18705]
                    :level 3:
                  title [18550-18576]
                    operator [18550-18554] ("*** ")
                    text [18554-18575] ("Пользовательский ввод")
                    newLine [18575-18576]
                  section [18576-18705]
                    srcBlock [18576-18665]
                        :language emacs-lisp:
                      blockHeader [18576-18598]
                        keyword [18576-18598]
                            :language emacs-lisp:
                          text [18576-18588] ("#+begin_src ")
                          srcLanguage [18588-18598] ("emacs-lisp")
                      newLine [18598-18599]
                      blockBody [18599-18655]
                        text [18599-18655] ("(message \\"U say: %s\\" (read-string \\"Say me something: \\"))")
                      newLine [18655-18656]
                      blockFooter [18656-18665]
                        keyword [18656-18665]
                          text [18656-18665] ("#+end_src")
                    newLine [18665-18666]
                    keyword [18666-18677]
                      text [18666-18677] ("#+RESULTS: ")
                    newLine [18677-18678]
                    fixedWidth [18678-18704]
                      operator [18678-18680] (": ")
                      text [18680-18704] ("U say: Ну че тут скажешь")
                    newLine [18704-18705]
                headline [18705-18891]
                    :level 3:
                  title [18705-18737]
                    operator [18705-18709] ("*** ")
                    text [18709-18736] ("Работа с выделенным текстом")
                    newLine [18736-18737]
                  section [18737-18891]
                    headline [18737-18788]
                        :level 4:
                      title [18737-18771]
                        operator [18737-18742] ("**** ")
                        text [18742-18770] ("Проверка что что-то выделено")
                        newLine [18770-18771]
                      section [18771-18788]
                        verbatim [18771-18787]
                          operator [18771-18772] ("=")
                          text [18772-18786] ("(use-region-p)")
                          operator [18786-18787] ("=")
                        newLine [18787-18788]
                    headline [18788-18891]
                        :level 4:
                      title [18788-18819]
                        operator [18788-18793] ("**** ")
                        text [18793-18818] ("Получить выделенный текст")
                        newLine [18818-18819]
                      section [18819-18891]
                        srcBlock [18819-18890]
                            :language emacs-lisp:
                          blockHeader [18819-18841]
                            keyword [18819-18841]
                                :language emacs-lisp:
                              text [18819-18831] ("#+begin_src ")
                              srcLanguage [18831-18841] ("emacs-lisp")
                          newLine [18841-18842]
                          blockBody [18842-18880]
                            text [18842-18880] ("(regionp (buffer-substring start end))")
                          newLine [18880-18881]
                          blockFooter [18881-18890]
                            keyword [18881-18890]
                              text [18881-18890] ("#+end_src")
                        newLine [18890-18891]
                headline [18891-19110]
                    :level 3:
                  title [18891-18937]
                    operator [18891-18895] ("*** ")
                    text [18895-18936] ("Конвертация символа в строку (ну и назад)")
                    newLine [18936-18937]
                  section [18937-19110]
                    srcBlock [18937-19066]
                        :language emacs-lisp:
                      blockHeader [18937-18959]
                        keyword [18937-18959]
                            :language emacs-lisp:
                          text [18937-18949] ("#+begin_src ")
                          srcLanguage [18949-18959] ("emacs-lisp")
                      newLine [18959-18960]
                      blockBody [18960-19056]
                        text [18960-19056] ("(symbol-name 'something) ;; Символ в строку\\n(intern (symbol-name 'something)) ;; Строка в символ")
                      newLine [19056-19057]
                      blockFooter [19057-19066]
                        keyword [19057-19066]
                          text [19057-19066] ("#+end_src")
                    newLine [19066-19067]
                    keyword [19067-19078]
                      text [19067-19078] ("#+RESULTS: ")
                    newLine [19078-19079]
                    fixedWidth [19079-19090]
                      operator [19079-19081] (": ")
                      text [19081-19090] ("something")
                    newLine [19090-19091]
                    newLine [19091-19092]
                    keyword [19092-19108]
                      text [19092-19100] ("#+CLOSE_")
                      text [19100-19108] ("{SPOILER")
                    text [19108-19109] ("}")
                    newLine [19109-19110]
                headline [19110-20191]
                    :level 3:
                  title [19110-19122]
                    operator [19110-19114] ("*** ")
                    text [19114-19121] ("Overlay")
                    newLine [19121-19122]
                  section [19122-20191]
                    text [19122-19302] ("Overlay это очень крутая тема. Он позволяет рендерить текст который не изменяет контент реального буфера. Это может быть полезно для показа подсказок, дебага, расчитанных значений.")
                    newLine [19302-19303]
                    newLine [19303-19304]
                    headline [19304-19453]
                        :level 4:
                      title [19304-19341]
                        operator [19304-19309] ("**** ")
                        text [19309-19340] ("Создание оверлея в конце строки")
                        newLine [19340-19341]
                      section [19341-19453]
                        srcBlock [19341-19452]
                            :language emacs-lisp:
                          blockHeader [19341-19363]
                            keyword [19341-19363]
                                :language emacs-lisp:
                              text [19341-19353] ("#+begin_src ")
                              srcLanguage [19353-19363] ("emacs-lisp")
                          newLine [19363-19364]
                          blockBody [19364-19442]
                            text [19364-19442] ("(setq my-first-overlay (make-overlay (line-end-position) (line-end-position)))")
                          newLine [19442-19443]
                          blockFooter [19443-19452]
                            keyword [19443-19452]
                              text [19443-19452] ("#+end_src")
                        newLine [19452-19453]
                    headline [19453-19748]
                        :level 4:
                      title [19453-19491]
                        operator [19453-19458] ("**** ")
                        text [19458-19490] ("Курсор заходит за предел оверлея")
                        newLine [19490-19491]
                      section [19491-19748]
                        text [19491-19621] ("В моем случае курсор выходил за предел оверлея. Решается весьма просто: вставляемый в оверлей текст необходимо наделить свойством ")
                        verbatim [19621-19632]
                          operator [19621-19622] ("=")
                          text [19622-19631] ("'cursor t")
                          operator [19631-19632] ("=")
                        newLine [19632-19633]
                        newLine [19633-19634]
                        srcBlock [19634-19747]
                            :language emacs-lisp:
                          blockHeader [19634-19656]
                            keyword [19634-19656]
                                :language emacs-lisp:
                              text [19634-19646] ("#+begin_src ")
                              srcLanguage [19646-19656] ("emacs-lisp")
                          newLine [19656-19657]
                          blockBody [19657-19737]
                            text [19657-19737] ("(setq my-popup-message (propertize popup-message 'face 'blamer--face 'cursor t))")
                          newLine [19737-19738]
                          blockFooter [19738-19747]
                            keyword [19738-19747]
                              text [19738-19747] ("#+end_src")
                        newLine [19747-19748]
                    headline [19748-20058]
                        :level 4:
                      title [19748-19779]
                        operator [19748-19753] ("**** ")
                        text [19753-19778] ("Изменение свойств overlay")
                        newLine [19778-19779]
                      section [19779-20058]
                        srcBlock [19779-20057]
                            :language emacs-lisp:
                          blockHeader [19779-19801]
                            keyword [19779-19801]
                                :language emacs-lisp:
                              text [19779-19791] ("#+begin_src ")
                              srcLanguage [19791-19801] ("emacs-lisp")
                          newLine [19801-19802]
                          blockBody [19802-20047]
                            text [19802-20047] ("    (overlay-put blamer--current-overlay 'after-string my-popup-message)\\n    (overlay-put blamer--current-overlay 'intangible t)\\n    (overlay-put blamer--current-overlay 'face 'bold)\\n    (overlay-put blamer--current-overlay 'cursor-intangible t)")
                          newLine [20047-20048]
                          blockFooter [20048-20057]
                            keyword [20048-20057]
                              text [20048-20057] ("#+end_src")
                        newLine [20057-20058]
                    headline [20058-20191]
                        :level 4:
                      title [20058-20094]
                        operator [20058-20063] ("**** ")
                        text [20063-20093] ("Удаление существующего оверлея")
                        newLine [20093-20094]
                      section [20094-20191]
                        srcBlock [20094-20190]
                            :language emacs-lisp:
                          blockHeader [20094-20116]
                            keyword [20094-20116]
                                :language emacs-lisp:
                              text [20094-20106] ("#+begin_src ")
                              srcLanguage [20106-20116] ("emacs-lisp")
                          newLine [20116-20117]
                          blockBody [20117-20180]
                            text [20117-20180] ("(if my-first-overlay\\n        (delete-overlay my-first-overlay))")
                          newLine [20180-20181]
                          blockFooter [20181-20190]
                            keyword [20181-20190]
                              text [20181-20190] ("#+end_src")
                        newLine [20190-20191]
                headline [20191-20336]
                    :level 3:
                  title [20191-20228]
                    operator [20191-20195] ("*** ")
                    text [20195-20222] ("Создание своего minor-mode ")
                    tagList [20222-20227]
                      operator [20222-20223] (":")
                      text [20223-20226] ("WIP")
                      operator [20226-20227] (":")
                    newLine [20227-20228]
                  section [20228-20336]
                    link [20228-20335]
                        :linkType raw:
                      operator [20228-20229] ("[")
                      linkUrl [20229-20320]
                        operator [20229-20230] ("[")
                        text [20230-20319] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html")
                        operator [20319-20320] ("]")
                      linkName [20320-20334]
                        operator [20320-20321] ("[")
                        text [20321-20333] ("Документация")
                        operator [20333-20334] ("]")
                      operator [20334-20335] ("]")
                    newLine [20335-20336]
            headline [20336-20725]
                :level 2:
              title [20336-20355]
                operator [20336-20339] ("** ")
                text [20339-20354] ("Работа с датами")
                newLine [20354-20355]
              section [20355-20725]
                link [20355-20480]
                    :linkType raw:
                  operator [20355-20356] ("[")
                  linkUrl [20356-20452]
                    operator [20356-20357] ("[")
                    text [20357-20451] ("htest-varps://stackoverflow.com/questions/4242012/how-do-i-add-dates-in-emacs-using-emacs-lisp")
                    operator [20451-20452] ("]")
                  linkName [20452-20479]
                    operator [20452-20453] ("[")
                    text [20453-20478] ("Ага, любимый стаковерфлоу")
                    operator [20478-20479] ("]")
                  operator [20479-20480] ("]")
                newLine [20480-20481]
                newLine [20481-20482]
                srcBlock [20482-20572]
                    :language emacs-lisp:
                  blockHeader [20482-20504]
                    keyword [20482-20504]
                        :language emacs-lisp:
                      text [20482-20494] ("#+begin_src ")
                      srcLanguage [20494-20504] ("emacs-lisp")
                  newLine [20504-20505]
                  blockBody [20505-20562]
                    text [20505-20562] ("(setq t3 (time-subtract (current-time) (days-to-time 2)))")
                  newLine [20562-20563]
                  blockFooter [20563-20572]
                    keyword [20563-20572]
                      text [20563-20572] ("#+end_src")
                newLine [20572-20573]
                table [20573-20602]
                  tableRow [20573-20602]
                    operator [20573-20574] ("|")
                    tableCell [20574-20581]
                      text [20574-20581] (" 24939 ")
                    operator [20581-20582] ("|")
                    tableCell [20582-20588]
                      text [20582-20588] (" 1255 ")
                    operator [20588-20589] ("|")
                    tableCell [20589-20597]
                      text [20589-20597] (" 721279 ")
                    operator [20597-20598] ("|")
                    tableCell [20598-20601]
                      text [20598-20601] (" 0 ")
                    operator [20601-20602] ("|")
                newLine [20602-20603]
                srcBlock [20603-20691]
                    :language emacs-lisp:
                  blockHeader [20603-20625]
                    keyword [20603-20625]
                        :language emacs-lisp:
                      text [20603-20615] ("#+begin_src ")
                      srcLanguage [20615-20625] ("emacs-lisp")
                  newLine [20625-20626]
                  blockBody [20626-20681]
                    text [20626-20681] ("\\n(message \\"%s\\" (/ (float-time (time-since t3)) (* 60)))")
                  newLine [20681-20682]
                  blockFooter [20682-20691]
                    keyword [20682-20691]
                      text [20682-20691] ("#+end_src")
                newLine [20691-20692]
                keyword [20692-20703]
                  text [20692-20703] ("#+RESULTS: ")
                newLine [20703-20704]
                fixedWidth [20704-20724]
                  operator [20704-20706] (": ")
                  text [20706-20724] ("2940.0710639333333")
                newLine [20724-20725]
            headline [20725-21863]
                :level 2:
              title [20725-20735]
                operator [20725-20728] ("** ")
                text [20728-20734] ("Regexp")
                newLine [20734-20735]
              section [20735-21863]
                headline [20735-21463]
                    :level 3:
                  title [20735-20747]
                    operator [20735-20739] ("*** ")
                    text [20739-20746] ("Примеры")
                    newLine [20746-20747]
                  section [20747-21463]
                    text [20747-20792] ("Просто кучка примеров из разработанного мной ")
                    link [20792-20848]
                        :linkType raw:
                      operator [20792-20793] ("[")
                      linkUrl [20793-20839]
                        operator [20793-20794] ("[")
                        text [20794-20838] ("htest-varps://github.com/Artawower/turbo-log")
                        operator [20838-20839] ("]")
                      linkName [20839-20847]
                        operator [20839-20840] ("[")
                        text [20840-20846] ("пакета")
                        operator [20846-20847] ("]")
                      operator [20847-20848] ("]")
                    text [20848-21024] (". Регекспы весьма похожи на то что представлено в других языках. Сложно лишь работать с интерполяцией строк (неочевидна работа с большим количеством слешей в исполняемом коде.)")
                    newLine [21024-21025]
                    newLine [21025-21026]
                    srcBlock [21026-21128]
                        :language emacs-lisp:
                      blockHeader [21026-21048]
                        keyword [21026-21048]
                            :language emacs-lisp:
                          text [21026-21038] ("#+begin_src ")
                          srcLanguage [21038-21048] ("emacs-lisp")
                      newLine [21048-21049]
                      blockBody [21049-21118]
                        text [21049-21118] ("(string-match \\"^\\\\([[:blank:]]\\\\)*\\\\(return\\\\)\\" \\"  return {\\n  name: 2\\n}\\")")
                      newLine [21118-21119]
                      blockFooter [21119-21128]
                        keyword [21119-21128]
                          text [21119-21128] ("#+end_src")
                    newLine [21128-21129]
                    keyword [21129-21140]
                      text [21129-21140] ("#+RESULTS: ")
                    newLine [21140-21141]
                    fixedWidth [21141-21144]
                      operator [21141-21143] (": ")
                      text [21143-21144] ("0")
                    newLine [21144-21145]
                    newLine [21145-21146]
                    newLine [21146-21147]
                    srcBlock [21147-21278]
                        :language emacs-lisp:
                      blockHeader [21147-21169]
                        keyword [21147-21169]
                            :language emacs-lisp:
                          text [21147-21159] ("#+begin_src ")
                          srcLanguage [21159-21169] ("emacs-lisp")
                      newLine [21169-21170]
                      blockBody [21170-21268]
                        text [21170-21268] ("(replace-regexp-in-string \\"[[:blank:]]*=[[:blank:]]*.+\\" \\"\\" \\"    this.myVariable = somethingElse;\\")")
                      newLine [21268-21269]
                      blockFooter [21269-21278]
                        keyword [21269-21278]
                          text [21269-21278] ("#+end_src")
                    newLine [21278-21279]
                    srcBlock [21279-21431]
                        :language emacs-lisp:
                      blockHeader [21279-21301]
                        keyword [21279-21301]
                            :language emacs-lisp:
                          text [21279-21291] ("#+begin_src ")
                          srcLanguage [21291-21301] ("emacs-lisp")
                      newLine [21301-21302]
                      blockBody [21302-21421]
                        text [21302-21421] ("(replace-regexp-in-string \\"\\\\(const\\\\|let\\\\|public\\\\|protected\\\\|private\\\\|var\\\\)[[:blank:]]*\\" \\"\\" \\"let anotherOne = userName\\")")
                      newLine [21421-21422]
                      blockFooter [21422-21431]
                        keyword [21422-21431]
                          text [21422-21431] ("#+end_src")
                    newLine [21431-21432]
                    keyword [21432-21443]
                      text [21432-21443] ("#+RESULTS: ")
                    newLine [21443-21444]
                    fixedWidth [21444-21462]
                      operator [21444-21446] (": ")
                      text [21446-21462] ("iable = userName")
                    newLine [21462-21463]
                headline [21463-21863]
                    :level 3:
                  title [21463-21489]
                    operator [21463-21467] ("*** ")
                    text [21467-21488] ("Regexp с группировкой")
                    newLine [21488-21489]
                  section [21489-21863]
                    srcBlock [21489-21664]
                        :language emacs-lisp:
                      blockHeader [21489-21511]
                        keyword [21489-21511]
                            :language emacs-lisp:
                          text [21489-21501] ("#+begin_src ")
                          srcLanguage [21501-21511] ("emacs-lisp")
                      newLine [21511-21512]
                      blockBody [21512-21654]
                        text [21512-21654] ("(concat \\"^(?\\\\(?1:[^s]+\\\\) [^s]\\n]+\\\\)\\"\\n          \\"s\\\\(?3:[0-9]\\\\{4\\\\}-[0-9]\\\\{2\\\\}-[0-9]\\\\{2\\\\}\\\\)\\"\\n          \\"s\\\\(?4:[0-9]\\\\{2\\\\}:[0-9]\\\\{2\\\\}:[0-9]\\\\{2\\\\}\\\\)\\")")
                      newLine [21654-21655]
                      blockFooter [21655-21664]
                        keyword [21655-21664]
                          text [21655-21664] ("#+end_src")
                    newLine [21664-21665]
                    srcBlock [21665-21844]
                        :language emacs-lisp:
                      blockHeader [21665-21687]
                        keyword [21665-21687]
                            :language emacs-lisp:
                          text [21665-21677] ("#+begin_src ")
                          srcLanguage [21677-21687] ("emacs-lisp")
                      newLine [21687-21688]
                      blockBody [21688-21834]
                        text [21688-21834] ("(setq test-string \\"feature/VW-221\\")\\n(string-match \\"\\\\(?1:[A-Za-z0-9]+/\\\\)\\\\(?2:VW-[0-9]\\\\)\\" test-string)\\n(message \\"res \\" (match-string 1 test-string))")
                      newLine [21834-21835]
                      blockFooter [21835-21844]
                        keyword [21835-21844]
                          text [21835-21844] ("#+end_src")
                    newLine [21844-21845]
                    keyword [21845-21856]
                      text [21845-21856] ("#+RESULTS: ")
                    newLine [21856-21857]
                    fixedWidth [21857-21862]
                      operator [21857-21859] (": ")
                      text [21859-21862] ("res")
                    newLine [21862-21863]
            headline [21863-21963]
                :level 2:
              title [21863-21883]
                operator [21863-21866] ("** ")
                text [21866-21882] ("Стандартные хуки")
                newLine [21882-21883]
              section [21883-21963]
                link [21883-21962]
                    :linkType raw:
                  operator [21883-21884] ("[")
                  linkUrl [21884-21941]
                    operator [21884-21885] ("[")
                    text [21885-21940] ("htest-varps://runebook.dev/ru/docs/elisp/standard-hooks")
                    operator [21940-21941] ("]")
                  linkName [21941-21961]
                    operator [21941-21942] ("[")
                    text [21942-21960] ("Просто смотри сюда")
                    operator [21960-21961] ("]")
                  operator [21961-21962] ("]")
                newLine [21962-21963]
            headline [21963-22624]
                :level 2:
              title [21963-21979]
                operator [21963-21966] ("** ")
                text [21966-21978] ("Custom modes")
                newLine [21978-21979]
              section [21979-22624]
                headline [21979-22624]
                    :level 3:
                  title [21979-21994]
                    operator [21979-21983] ("*** ")
                    text [21983-21993] ("Minor mode")
                    newLine [21993-21994]
                  section [21994-22624]
                    text [21994-22101] ("Для того чтобы сделать свой minor mode достаточно его объявить и описать логику включения/выключений режима")
                    newLine [22101-22102]
                    newLine [22102-22103]
                    srcBlock [22103-22448]
                        :language emacs-lisp:
                      blockHeader [22103-22125]
                        keyword [22103-22125]
                            :language emacs-lisp:
                          text [22103-22115] ("#+begin_src ")
                          srcLanguage [22115-22125] ("emacs-lisp")
                      newLine [22125-22126]
                      blockBody [22126-22438]
                        text [22126-22438] (";;;###autoload\\n(define-minor-mode wakatime-ui-mode\\n  \\"Wakatime ui mode. Add time track to doom modeline.\\nTODO:\\nAdd support for other modeline in future.\\"\\n  :init-value nil\\n  :global t\\n  :lighter nil\\n  :group 'wakatime-ui\\n  (if wakatime-ui-mode\\n      (wakatime-ui--watch-time)\\n    (wakatime-ui--stop-watch-time)))")
                      newLine [22438-22439]
                      blockFooter [22439-22448]
                        keyword [22439-22448]
                          text [22439-22448] ("#+end_src")
                    newLine [22448-22449]
                    text [22449-22453] ("Где:")
                    newLine [22453-22454]
                    newLine [22454-22455]
                    verbatim [22455-22467]
                      operator [22455-22456] ("=")
                      text [22456-22466] ("init-value")
                      operator [22466-22467] ("=")
                    text [22467-22491] (" - значение по умолчанию")
                    newLine [22491-22492]
                    verbatim [22492-22500]
                      operator [22492-22493] ("=")
                      text [22493-22499] ("global")
                      operator [22499-22500] ("=")
                    text [22500-22556] (" - должен ли быть вызван глобальный мод перед локальным?")
                    newLine [22556-22557]
                    verbatim [22557-22566]
                      operator [22557-22558] ("=")
                      text [22558-22565] ("lighter")
                      operator [22565-22566] ("=")
                    text [22566-22623] (" - определяет что отображать в modeline когда мод включен")
                    newLine [22623-22624]
            headline [22624-22694]
                :level 2:
              title [22624-22634]
                operator [22624-22627] ("** ")
                text [22627-22633] ("Window")
                newLine [22633-22634]
              section [22634-22694]
                headline [22634-22694]
                    :level 3:
                  title [22634-22671]
                    operator [22634-22638] ("*** ")
                    text [22638-22670] ("Получение ширины текущего экрана")
                    newLine [22670-22671]
                  section [22671-22694]
                    verbatim [22671-22693]
                      operator [22671-22672] ("=")
                      text [22672-22692] ("(window-total-width)")
                      operator [22692-22693] ("=")
                    newLine [22693-22694]
            headline [22694-23195]
                :level 2:
              title [22694-22730]
                operator [22694-22697] ("** ")
                text [22697-22729] ("Асинхронное исполнение. Process.")
                newLine [22729-22730]
              section [22730-23195]
                text [22730-22762] ("Создание асинхронного процесаа (")
                link [22762-22873]
                    :linkType raw:
                  operator [22762-22763] ("[")
                  linkUrl [22763-22856]
                    operator [22763-22764] ("[")
                    text [22764-22855] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html")
                    operator [22855-22856] ("]")
                  linkName [22856-22872]
                    operator [22856-22857] ("[")
                    text [22857-22871] ("подробнее тут)")
                    operator [22871-22872] ("]")
                  operator [22872-22873] ("]")
                newLine [22873-22874]
                newLine [22874-22875]
                srcBlock [22875-23124]
                    :language emacs-lisp:
                  blockHeader [22875-22897]
                    keyword [22875-22897]
                        :language emacs-lisp:
                      text [22875-22887] ("#+begin_src ")
                      srcLanguage [22887-22897] ("emacs-lisp")
                  newLine [22897-22898]
                  blockBody [22898-23114]
                    text [22898-23114] ("(setq process (start-process\\n                    \\"WakatimeUI\\"\\n                    wakatime-ui--buffer\\n                    (wakatime-find-binary)\\n                    (plist-get wakatime-ui--command-args :today-time)))")
                  newLine [23114-23115]
                  blockFooter [23115-23124]
                    keyword [23115-23124]
                      text [23115-23124] ("#+end_src")
                newLine [23124-23125]
                text [23125-23159] ("Чтение выходных данных из процесса")
                newLine [23159-23160]
                newLine [23160-23161]
                srcBlock [23161-23194]
                    :language emacs-lisp:
                  blockHeader [23161-23183]
                    keyword [23161-23183]
                        :language emacs-lisp:
                      text [23161-23173] ("#+begin_src ")
                      srcLanguage [23173-23183] ("emacs-lisp")
                  newLine [23183-23184]
                  newLine [23184-23185]
                  blockFooter [23185-23194]
                    keyword [23185-23194]
                      text [23185-23194] ("#+end_src")
                newLine [23194-23195]
            headline [23195-23824]
                :level 2:
              title [23195-23206]
                operator [23195-23198] ("** ")
                text [23198-23205] ("Keymaps")
                newLine [23205-23206]
              section [23206-23824]
                headline [23206-23824]
                    :level 3:
                  title [23206-23233]
                    operator [23206-23210] ("*** ")
                    text [23210-23232] ("Создание своего keymap")
                    newLine [23232-23233]
                  section [23233-23824]
                    srcBlock [23233-23812]
                        :language elisp:
                      blockHeader [23233-23250]
                        keyword [23233-23250]
                            :language elisp:
                          text [23233-23245] ("#+begin_src ")
                          srcLanguage [23245-23250] ("elisp")
                      newLine [23250-23251]
                      blockBody [23251-23802]
                        text [23251-23802] ("(with-current-buffer \\"*Messages*\\"\\n  (read-only-mode -1)\\n  (erase-buffer))\\n\\n(setq my-mode-map (make-sparse-keymap))\\n(define-key my-mode-map (kbd \\"C-c C-'\\") 'my-mode-cmd1)\\n(define-key my-mode-map (kbd \\"C-c C-b\\") 'my-mode-cmd2)\\n(define-key my-mode-map (kbd \\"C-c C-c\\") 'my-mode-cmd3)\\n(define-key my-mode-map (kbd \\"<mouse-1>\\") 'my-mode-cmd4)\\n;; by convention, major mode's keys should begin with the form C-c C-‹key›\\n\\n;; (dolist (m my-mode-map)\\n;;   (message \\"key: %s\\" m))\\n\\n\\n\\n\\n\\n(map-keymap '(lambda (v g)\\n               (message \\"%s: %s\\" v g)) my-mode-map)")
                      newLine [23802-23803]
                      blockFooter [23803-23812]
                        keyword [23803-23812]
                          text [23803-23812] ("#+end_src")
                    newLine [23812-23813]
                    keyword [23813-23823]
                      text [23813-23823] ("#+RESULTS:")
                    newLine [23823-23824]
            headline [23824-28959]
                :level 2:
                :id  elisp-macros:
              title [23824-23833]
                operator [23824-23827] ("** ")
                text [23827-23832] ("Macro")
                newLine [23832-23833]
              section [23833-28959]
                propertyDrawer [23833-23869]
                  property [23833-23845]
                    text [23833-23845] (":PROPERTIES:")
                  newLine [23845-23846]
                  property [23846-23863]
                    text [23846-23851] (":ID: ")
                    text [23851-23863] ("elisp-macros")
                  newLine [23863-23864]
                  property [23864-23869]
                    text [23864-23869] (":END:")
                newLine [23869-23870]
                text [23870-23880] ("Подробнее ")
                link [23880-23964]
                    :linkType raw:
                  operator [23880-23881] ("[")
                  linkUrl [23881-23958]
                    operator [23881-23882] ("[")
                    text [23882-23957] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html")
                    operator [23957-23958] ("]")
                  linkName [23958-23963]
                    operator [23958-23959] ("[")
                    text [23959-23962] ("тут")
                    operator [23962-23963] ("]")
                  operator [23963-23964] ("]")
                text [23964-23965] (".")
                newLine [23965-23966]
                newLine [23966-23967]
                headline [23967-24141]
                    :level 3:
                  title [23967-23986]
                    operator [23967-23971] ("*** ")
                    text [23971-23985] ("Простой макрос")
                    newLine [23985-23986]
                  section [23986-24141]
                    srcBlock [23986-24123]
                        :language emacs-lisp:
                      blockHeader [23986-24008]
                        keyword [23986-24008]
                            :language emacs-lisp:
                          text [23986-23998] ("#+begin_src ")
                          srcLanguage [23998-24008] ("emacs-lisp")
                      newLine [24008-24009]
                      blockBody [24009-24113]
                        text [24009-24113] ("(defmacro inc (var)\\n  (list 'setq var (list '1+ var)))\\n\\n(setq test-var 10)\\n(message \\"%s\\" (inc test-var))")
                      newLine [24113-24114]
                      blockFooter [24114-24123]
                        keyword [24114-24123]
                          text [24114-24123] ("#+end_src")
                    newLine [24123-24124]
                    keyword [24124-24135]
                      text [24124-24135] ("#+RESULTS: ")
                    newLine [24135-24136]
                    fixedWidth [24136-24140]
                      operator [24136-24138] (": ")
                      text [24138-24140] ("11")
                    newLine [24140-24141]
                headline [24141-24316]
                    :level 3:
                  title [24141-24160]
                    operator [24141-24145] ("*** ")
                    text [24145-24159] ("Изучить макрос")
                    newLine [24159-24160]
                  section [24160-24316]
                    text [24160-24198] ("Macroexpand - показывает вывод макроса")
                    newLine [24198-24199]
                    newLine [24199-24200]
                    srcBlock [24200-24277]
                        :language emacs-lisp:
                      blockHeader [24200-24222]
                        keyword [24200-24222]
                            :language emacs-lisp:
                          text [24200-24212] ("#+begin_src ")
                          srcLanguage [24212-24222] ("emacs-lisp")
                      newLine [24222-24223]
                      blockBody [24223-24267]
                        text [24223-24267] ("(message \\"%s\\" (macroexpand '(inc test-var)))")
                      newLine [24267-24268]
                      blockFooter [24268-24277]
                        keyword [24268-24277]
                          text [24268-24277] ("#+end_src")
                    newLine [24277-24278]
                    keyword [24278-24289]
                      text [24278-24289] ("#+RESULTS: ")
                    newLine [24289-24290]
                    fixedWidth [24290-24315]
                      operator [24290-24292] (": ")
                      text [24292-24315] ("(setq test-var (1+ tt))")
                    newLine [24315-24316]
                headline [24316-24655]
                    :level 3:
                  title [24316-24340]
                    operator [24316-24320] ("*** ")
                    text [24320-24339] ("Цепочка из макросов")
                    newLine [24339-24340]
                  section [24340-24655]
                    text [24340-24422] ("Macroexpand отображает только первый макрос, чтобы вызвать всю цепочку используем ")
                    verbatim [24422-24439]
                      operator [24422-24423] ("=")
                      text [24423-24438] ("macroexpand-all")
                      operator [24438-24439] ("=")
                    newLine [24439-24440]
                    newLine [24440-24441]
                    srcBlock [24441-24600]
                        :language emacs-lisp:
                      blockHeader [24441-24463]
                        keyword [24441-24463]
                            :language emacs-lisp:
                          text [24441-24453] ("#+begin_src ")
                          srcLanguage [24453-24463] ("emacs-lisp")
                      newLine [24463-24464]
                      blockBody [24464-24590]
                        text [24464-24590] ("(defmacro inc2 (var1 var2)\\n    (list 'progn (list 'inc var1) (list 'inc var2)))\\n\\n\\n(message \\"%s\\" (macroexpand-all '(inc2 r s)))")
                      newLine [24590-24591]
                      blockFooter [24591-24600]
                        keyword [24591-24600]
                          text [24591-24600] ("#+end_src")
                    newLine [24600-24601]
                    keyword [24601-24612]
                      text [24601-24612] ("#+RESULTS: ")
                    newLine [24612-24613]
                    fixedWidth [24613-24654]
                      operator [24613-24615] (": ")
                      text [24615-24654] ("(progn (setq r (1+ r)) (setq s (1+ s)))")
                    newLine [24654-24655]
                headline [24655-24863]
                    :level 3:
                  title [24655-24697]
                    operator [24655-24659] ("*** ")
                    text [24659-24696] ("Пример с более сложными конструкциями")
                    newLine [24696-24697]
                  section [24697-24863]
                    srcBlock [24697-24844]
                        :language emacs-lisp:
                      blockHeader [24697-24719]
                        keyword [24697-24719]
                            :language emacs-lisp:
                          text [24697-24709] ("#+begin_src ")
                          srcLanguage [24709-24719] ("emacs-lisp")
                      newLine [24719-24720]
                      blockBody [24720-24834]
                        text [24720-24834] ("(defmacro t-becomes-nil (var)\\n  \`(if (eq ,var t)\\n       (setq ,var nil)))\\n\\n(message \\"%s\\" (t-becomes-nil test-var))")
                      newLine [24834-24835]
                      blockFooter [24835-24844]
                        keyword [24835-24844]
                          text [24835-24844] ("#+end_src")
                    newLine [24844-24845]
                    keyword [24845-24856]
                      text [24845-24856] ("#+RESULTS: ")
                    newLine [24856-24857]
                    fixedWidth [24857-24862]
                      operator [24857-24859] (": ")
                      text [24859-24862] ("nil")
                    newLine [24862-24863]
                headline [24863-25972]
                    :level 3:
                  title [24863-24901]
                    operator [24863-24867] ("*** ")
                    text [24867-24900] ("Динамическое получение переменной")
                    newLine [24900-24901]
                  section [24901-25972]
                    indent [24901-24902] (" ")
                    link [24902-25023]
                        :linkType network:
                      operator [24902-24903] ("[")
                      linkUrl [24903-25005]
                        operator [24903-24904] ("[")
                        text [24904-25004] ("https://stackoverflow.com/questions/24433035/combining-two-variables-into-one-function-name-in-macro")
                        operator [25004-25005] ("]")
                      linkName [25005-25022]
                        operator [25005-25006] ("[")
                        text [25006-25021] ("Подсмотрено тут")
                        operator [25021-25022] ("]")
                      operator [25022-25023] ("]")
                    newLine [25023-25024]
                    indent [25024-25025] (" ")
                    text [25025-25042] ("Чертовая магия 😮")
                    newLine [25042-25043]
                    newLine [25043-25044]
                    srcBlock [25044-25282]
                        :language emacs-lisp:
                      blockHeader [25044-25066]
                        keyword [25044-25066]
                            :language emacs-lisp:
                          text [25044-25056] ("#+begin_src ")
                          srcLanguage [25056-25066] ("emacs-lisp")
                      newLine [25066-25067]
                      blockBody [25067-25272]
                        text [25067-25272] ("(setq my-custom-variable \\"Hello, amma variable\\")\\n\\n(defmacro get-with-prefix (var-name)\\n  \`(symbol-value (intern (concatenate 'string \\"my-custom\\" \\"-\\" (symbol-name ',var-name)))))\\n\\n(get-with-prefix variable)")
                      newLine [25272-25273]
                      blockFooter [25273-25282]
                        keyword [25273-25282]
                          text [25273-25282] ("#+end_src")
                    newLine [25282-25283]
                    keyword [25283-25294]
                      text [25283-25294] ("#+RESULTS: ")
                    newLine [25294-25295]
                    fixedWidth [25295-25317]
                      operator [25295-25297] (": ")
                      text [25297-25317] ("Hello, amma variable")
                    newLine [25317-25318]
                    newLine [25318-25319]
                    text [25319-25485] ("А теперь из plist, если нет - то из глобального скоупа, это еще большая магия. Да, наверное такое не стоит использовать в реальных проектах, но как же руки чешутся 😍")
                    newLine [25485-25486]
                    newLine [25486-25487]
                    srcBlock [25487-25936]
                        :language emacs-lisp:
                      blockHeader [25487-25509]
                        keyword [25487-25509]
                            :language emacs-lisp:
                          text [25487-25499] ("#+begin_src ")
                          srcLanguage [25499-25509] ("emacs-lisp")
                      newLine [25509-25510]
                      blockBody [25510-25926]
                        text [25510-25926] ("(setq my-custom-variable \\"Hello, amma variable\\")\\n\\n(setq my-plist-with-prop '(:custom-variable nil :test t))\\n\\n(defmacro get-with-prefix (my-plist var-name)\\n  \`(or (plist-get ,my-plist (symbol-value (intern (concatenate 'string \\":\\" (symbol-name ',var-name)))))\\n       (symbol-value (intern (concatenate 'string \\"my\\" \\"-\\" (symbol-name ',var-name))))))\\n\\n(message \\"%s\\" (get-with-prefix my-plist-with-prop custom-variable))")
                      newLine [25926-25927]
                      blockFooter [25927-25936]
                        keyword [25927-25936]
                          text [25927-25936] ("#+end_src")
                    newLine [25936-25937]
                    keyword [25937-25948]
                      text [25937-25948] ("#+RESULTS: ")
                    newLine [25948-25949]
                    fixedWidth [25949-25971]
                      operator [25949-25951] (": ")
                      text [25951-25971] ("Hello, amma variable")
                    newLine [25971-25972]
                headline [25972-27121]
                    :level 3:
                  title [25972-26009]
                    operator [25972-25976] ("*** ")
                    text [25976-25998] ("Передача тела (@body) ")
                    tagList [25998-26008]
                      operator [25998-25999] (":")
                      text [25999-26007] ("noexport")
                      operator [26007-26008] (":")
                    newLine [26008-26009]
                  section [26009-27121]
                    text [26009-26163] ("Пожалуй самая впечатлаяющая фича (имхо, без нее смысл в макросах бы отпал). Макрос склеивает результаты выполнения функций (подумал для org-mode самое то)")
                    newLine [26163-26164]
                    newLine [26164-26165]
                    srcBlock [26165-26494]
                        :language emacs-lisp:
                      blockHeader [26165-26187]
                        keyword [26165-26187]
                            :language emacs-lisp:
                          text [26165-26177] ("#+begin_src ")
                          srcLanguage [26177-26187] ("emacs-lisp")
                      newLine [26187-26188]
                      blockBody [26188-26484]
                        text [26188-26484] ("(setq test-var 0)\\n(defmacro for (var from init to final do &rest body)\\n  \`(let ((,var ,init))\\n     (while (<= ,var ,final)\\n       ,@body\\n       (setq ,var (1+ ,var)))))\\n\\n\\n(for j from 0 to 4 do\\n     (setq test-var (+ test-var j))\\n     (setq test-var (/ test-var 2)))\\n\\n(message \\"HAVA: %s\\" test-var)")
                      newLine [26484-26485]
                      blockFooter [26485-26494]
                        keyword [26485-26494]
                          text [26485-26494] ("#+end_src")
                    newLine [26494-26495]
                    keyword [26495-26506]
                      text [26495-26506] ("#+RESULTS: ")
                    newLine [26506-26507]
                    fixedWidth [26507-26516]
                      operator [26507-26509] (": ")
                      text [26509-26516] ("HAVA: 3")
                    newLine [26516-26517]
                    newLine [26517-26518]
                    newLine [26518-26519]
                    headline [26519-27121]
                        :level 4:
                      title [26519-26542]
                        operator [26519-26524] ("**** ")
                        text [26524-26531] ("Failed ")
                        tagList [26531-26541]
                          operator [26531-26532] (":")
                          text [26532-26540] ("noexport")
                          operator [26540-26541] (":")
                        newLine [26541-26542]
                      section [26542-27121]
                        text [26542-26611] ("Пример макроса, чтобы наглядно видеть в орге какая функция что делает")
                        newLine [26611-26612]
                        newLine [26612-26613]
                        srcBlock [26613-26937]
                            :language emacs-lisp:
                          blockHeader [26613-26635]
                            keyword [26613-26635]
                                :language emacs-lisp:
                              text [26613-26625] ("#+begin_src ")
                              srcLanguage [26625-26635] ("emacs-lisp")
                          newLine [26635-26636]
                          blockBody [26636-26927]
                            text [26636-26927] ("(defmacro pretty-log (&rest body)\\n\\n  (let ((res (concat (make-string 80 ?-) \\"\\n\\")))\\n    (dolist (f body)\\n      (setq res (concat res (format \\"[%s]: %s\\n\\" f (eval f)))))\\n    (message res)))\\n\\n(pretty-log (+ 1 12)\\n            (- 44 22)\\n            (+ (/ 12 2) (* 33 4))\\n            (setq ttt 12))")
                          newLine [26927-26928]
                          blockFooter [26928-26937]
                            keyword [26928-26937]
                              text [26928-26937] ("#+end_src")
                        newLine [26937-26938]
                        keyword [26938-26949]
                          text [26938-26949] ("#+RESULTS: ")
                        newLine [26949-26950]
                        fixedWidth [26950-27032]
                          operator [26950-26952] (": ")
                          text [26952-27032] ("--------------------------------------------------------------------------------")
                        newLine [27032-27033]
                        fixedWidth [27033-27049]
                          operator [27033-27035] (": ")
                          text [27035-27049] ("[(+ 1 12)]: 13")
                        newLine [27049-27050]
                        fixedWidth [27050-27067]
                          operator [27050-27052] (": ")
                          text [27052-27067] ("[(- 44 22)]: 22")
                        newLine [27067-27068]
                        fixedWidth [27068-27098]
                          operator [27068-27070] (": ")
                          text [27070-27098] ("[(+ (/ 12 2) (* 33 4))]: 138")
                        newLine [27098-27099]
                        fixedWidth [27099-27120]
                          operator [27099-27101] (": ")
                          text [27101-27120] ("[(setq ttt 12)]: 12")
                        newLine [27120-27121]
                headline [27121-28959]
                    :level 3:
                  title [27121-27209]
                    operator [27121-27125] ("*** ")
                    text [27125-27198] ("Модификация plist через список динамических аргументов как в use-package ")
                    tagList [27198-27208]
                      operator [27198-27199] (":")
                      text [27199-27207] ("noexport")
                      operator [27207-27208] (":")
                    newLine [27208-27209]
                  section [27209-28959]
                    srcBlock [27209-28734]
                        :language emacs-lisp:
                      blockHeader [27209-27231]
                        keyword [27209-27231]
                            :language emacs-lisp:
                          text [27209-27221] ("#+begin_src ")
                          srcLanguage [27221-27231] ("emacs-lisp")
                      newLine [27231-27232]
                      blockBody [27232-28724]
                        text [27232-28724] ("(setq res \\"\\")\\n(setq test-alist\\n      '((js-mode (:loggers '(\\"hi there\\") :msg-format-template \\"Hi\\" :argument-divider \\"|\\"))\\n        (typescript-mode (:loggers '(\\"another on\\", \\"and me\\") :msg-format-template \\"bee\\"))\\n        ))\\n\\n(defmacro turbo-log-configure (&rest configs)\\n  (let* ((strategy (or (plist-get configs :strategy) 'replace))\\n         (excluded-keys '(:modes :strategy))\\n         (modes (plist-get configs :modes))\\n         current-config)\\n\\n    (dolist (k excluded-keys)\\n      (setq configs (map-delete configs k)))\\n\\n    (dolist (mode modes)\\n      (unless (assoc mode test-alist)\\n        (push \`(,mode nil) test-alist))\\n\\n      (setq current-config (car (cdr-safe (assoc mode test-alist))))\\n\\n      (if (eq strategy 'replace)\\n          (setq current-config configs)\\n\\n        (loop for (k v) on configs by 'cddr do\\n              (if current-config\\n                  (plist-put current-config k v)\\n                (setq current-config \`(,k ,v)))))\\n\\n      (message \\"QQQ: %s\\" configs)\\n      (if (assq mode test-alist)\\n          (setcdr (assq mode test-alist)\\n                  \`(,current-config))\\n        \`(push '(,mode '(,current-config)) ,test-alist))\\n      )))\\n\\n(turbo-log-configure\\n :modes (typescript-mode js2-mode js-mode)\\n ;; :modes (typescript-mode j-mode)\\n ;; :modes (js-mode)\\n :strategy replace\\n\\n :loggers (\\"console.print\\" \\"console.dbg\\")\\n :msg-format-template \\"\\"HELLO WORLD: %s\\"\\")\\n\\n(message \\"-------------------------------------------------------\\")\\n(message \\"%s\\" (pp test-alist))")
                      newLine [28724-28725]
                      blockFooter [28725-28734]
                        keyword [28725-28734]
                          text [28725-28734] ("#+end_src")
                    newLine [28734-28735]
                    keyword [28735-28746]
                      text [28735-28746] ("#+RESULTS: ")
                    newLine [28746-28747]
                    fixedWidth [28747-28760]
                      operator [28747-28749] (": ")
                      text [28749-28760] ("((mode nil)")
                    newLine [28760-28761]
                    fixedWidth [28761-28772]
                      operator [28761-28763] (": ")
                      text [28763-28772] (" (js-mode")
                    newLine [28772-28773]
                    fixedWidth [28773-28786]
                      operator [28773-28775] (": ")
                      text [28775-28786] ("  (:loggers")
                    newLine [28786-28787]
                    fixedWidth [28787-28805]
                      operator [28787-28789] (": ")
                      text [28789-28805] ("   '(\\"hi there\\")")
                    newLine [28805-28806]
                    fixedWidth [28806-28838]
                      operator [28806-28808] (": ")
                      text [28808-28838] ("   :msg-format-template \\"Hi\\"))")
                    newLine [28838-28839]
                    fixedWidth [28839-28858]
                      operator [28839-28841] (": ")
                      text [28841-28858] (" (typescript-mode")
                    newLine [28858-28859]
                    fixedWidth [28859-28872]
                      operator [28859-28861] (": ")
                      text [28861-28872] ("  (:loggers")
                    newLine [28872-28873]
                    fixedWidth [28873-28909]
                      operator [28873-28875] (": ")
                      text [28875-28909] ("   (\\"console.print\\" \\"console.dbg\\")")
                    newLine [28909-28910]
                    fixedWidth [28910-28958]
                      operator [28910-28912] (": ")
                      text [28912-28958] ("   :msg-format-template \\"\\"HELLO WORLD: %s\\"\\")))")
                    newLine [28958-28959]
        headline [28959-29391]
            :level 1:
          title [28959-28984]
            operator [28959-28961] ("* ")
            text [28961-28983] ("Создание своего пакета")
            newLine [28983-28984]
          section [28984-29391]
            headline [28984-29146]
                :level 2:
              title [28984-29014]
                operator [28984-28987] ("** ")
                text [28987-29013] ("Проверка ошибок компиляции")
                newLine [29013-29014]
              section [29014-29146]
                srcBlock [29014-29145]
                    :language bash:
                  blockHeader [29014-29030]
                    keyword [29014-29030]
                        :language bash:
                      text [29014-29026] ("#+begin_src ")
                      srcLanguage [29026-29030] ("bash")
                  newLine [29030-29031]
                  blockBody [29031-29135]
                    text [29031-29135] ("emacs -Q --batch     --eval '(setq byte-compile-error-on-warn t)'     -f batch-byte-compile turbo-log.el")
                  newLine [29135-29136]
                  blockFooter [29136-29145]
                    keyword [29136-29145]
                      text [29136-29145] ("#+end_src")
                newLine [29145-29146]
            headline [29146-29209]
                :level 2:
              title [29146-29160]
                operator [29146-29149] ("** ")
                text [29149-29159] ("Contribute")
                newLine [29159-29160]
              section [29160-29209]
                link [29160-29208]
                    :linkType raw:
                  operator [29160-29161] ("[")
                  linkUrl [29161-29207]
                    operator [29161-29162] ("[")
                    text [29162-29206] ("htest-varps://github.com/leotaku/elisp-check")
                    operator [29206-29207] ("]")
                  operator [29207-29208] ("]")
                newLine [29208-29209]
            headline [29209-29391]
                :level 2:
              title [29209-29215]
                operator [29209-29212] ("** ")
                text [29212-29214] ("CI")
                newLine [29214-29215]
              section [29215-29391]
                link [29215-29324]
                    :linkType raw:
                  operator [29215-29216] ("[")
                  linkUrl [29216-29300]
                    operator [29216-29217] ("[")
                    text [29217-29299] ("htest-varps://github.com/a13/reverse-im.el/blob/master/.github/workflows/check.yml")
                    operator [29299-29300] ("]")
                  linkName [29300-29323]
                    operator [29300-29301] ("[")
                    text [29301-29322] ("Пример github actions")
                    operator [29322-29323] ("]")
                  operator [29323-29324] ("]")
                newLine [29324-29325]
                link [29325-29390]
                    :linkType raw:
                  operator [29325-29326] ("[")
                  linkUrl [29326-29372]
                    operator [29326-29327] ("[")
                    text [29327-29371] ("htest-varps://github.com/leotaku/elisp-check")
                    operator [29371-29372] ("]")
                  linkName [29372-29389]
                    operator [29372-29373] ("[")
                    text [29373-29388] ("Про elisp check")
                    operator [29388-29389] ("]")
                  operator [29389-29390] ("]")
                newLine [29390-29391]
        headline [29391-30257]
            :level 1:
          title [29391-29399]
            operator [29391-29393] ("* ")
            text [29393-29398] ("Тесты")
            newLine [29398-29399]
          section [29399-30257]
            text [29399-29528] ("Тесты пишутся весьма просто. От части потому что не нужно мокать кучу зависимостей. Функция в большинстве случаев самодостаточна.")
            newLine [29528-29529]
            newLine [29529-29530]
            srcBlock [29530-29621]
                :language emacs-lisp:
              blockHeader [29530-29552]
                keyword [29530-29552]
                    :language emacs-lisp:
                  text [29530-29542] ("#+begin_src ")
                  srcLanguage [29542-29552] ("emacs-lisp")
              newLine [29552-29553]
              blockBody [29553-29611]
                text [29553-29611] ("(ert-deftest my-first-test ()\\n  (should (= (+ 10 10) 20)))")
              newLine [29611-29612]
              blockFooter [29612-29621]
                keyword [29612-29621]
                  text [29612-29621] ("#+end_src")
            newLine [29621-29622]
            text [29622-29629] ("Запуск.")
            newLine [29629-29630]
            newLine [29630-29631]
            srcBlock [29631-29734]
                :language bash:
              blockHeader [29631-29647]
                keyword [29631-29647]
                    :language bash:
                  text [29631-29643] ("#+begin_src ")
                  srcLanguage [29643-29647] ("bash")
              newLine [29647-29648]
              blockBody [29648-29724]
                text [29648-29724] ("emacs -batch -l ert -l package.el -l test.el -f ert-run-tests-batch-and-exit")
              newLine [29724-29725]
              blockFooter [29725-29734]
                keyword [29725-29734]
                  text [29725-29734] ("#+end_src")
            newLine [29734-29735]
            undefined [29735-29764]
                :language {HIDDEN:
              blockHeader [29735-29750]
                keyword [29735-29750]
                    :language {HIDDEN:
                  text [29735-29743] ("#+BEGIN_")
                  srcLanguage [29743-29750] ("{HIDDEN")
              newLine [29750-29751]
              blockBody [29751-29751]
              blockFooter [29751-29764]
                keyword [29751-29764]
                  text [29751-29757] ("#+END_")
                  text [29757-29764] ("{HIDDEN")
            text [29764-29765] ("}")
            newLine [29765-29766]
            newLine [29766-29767]
            srcBlock [29767-29867]
                :language emacs-lisp:
              blockHeader [29767-29789]
                keyword [29767-29789]
                    :language emacs-lisp:
                  text [29767-29779] ("#+begin_src ")
                  srcLanguage [29779-29789] ("emacs-lisp")
              newLine [29789-29790]
              blockBody [29790-29857]
                text [29790-29857] ("(setq v (dolist (i '(1 2 3 4))\\n                i))\\n(message \\"%s\\" v)")
              newLine [29857-29858]
              blockFooter [29858-29867]
                keyword [29858-29867]
                  text [29858-29867] ("#+end_src")
            newLine [29867-29868]
            keyword [29868-29879]
              text [29868-29879] ("#+RESULTS: ")
            newLine [29879-29880]
            fixedWidth [29880-29885]
              operator [29880-29882] (": ")
              text [29882-29885] ("nil")
            newLine [29885-29886]
            newLine [29886-29887]
            newLine [29887-29888]
            headline [29888-30257]
                :level 2:
              title [29888-29902]
                operator [29888-29891] ("** ")
                text [29891-29901] ("Check json")
                newLine [29901-29902]
              section [29902-30257]
                srcBlock [29902-30246]
                    :language emacs-lisp:
                  blockHeader [29902-29924]
                    keyword [29902-29924]
                        :language emacs-lisp:
                      text [29902-29914] ("#+begin_src ")
                      srcLanguage [29914-29924] ("emacs-lisp")
                  newLine [29924-29925]
                  blockBody [29925-30236]
                    text [29925-30236] ("  (let* ((json-object-type 'plist)\\n         (json-array-type 'list)\\n         (json-key-type 'string)\\n         (json (json-read-file web-roam-configuration-file-path))\\n         (name-to-config (make-hash-table :test 'equal))\\n         (server-names '()))\\n    (dolist (config json)\\n      (message \\"%s\\" config))\\n  )")
                  newLine [30236-30237]
                  blockFooter [30237-30246]
                    keyword [30237-30246]
                      text [30237-30246] ("#+end_src")
                newLine [30246-30247]
                keyword [30247-30257]
                  text [30247-30257] ("#+RESULTS:")
        headline [30257-30353]
            :level 1:
          title [29752-29779]
            operator [29752-29754] ("* ")
            text [29754-29778] ("Статический анализ типов")
            newLine [29778-29779]
          section [29779-29848]
            link [29779-29847]
                :linkType network:
              operator [29779-29780] ("[")
              linkUrl [29780-29816]
                operator [29780-29781] ("[")
                text [29781-29815] ("https://github.com/emacs-elsa/Elsa")
                operator [29815-29816] ("]")
              linkName [29816-29846]
                operator [29816-29817] ("[")
                text [29817-29845] ("Его нет. Зато есть аннотации")
                operator [29845-29846] ("]")
              operator [29846-29847] ("]")
            newLine [29847-29848]
        headline [30353-32238]
            :level 1:
          title [29848-29870]
            operator [29848-29850] ("* ")
            text [29850-29859] ("Временно ")
            tagList [29859-29869]
              operator [29859-29860] (":")
              text [29860-29868] ("noexport")
              operator [29868-29869] (":")
            newLine [29869-29870]
          section [29870-31733]
            srcBlock [29870-29931]
                :language emacs-lisp:
              blockHeader [29870-29892]
                keyword [29870-29892]
                    :language emacs-lisp:
                  text [29870-29882] ("#+begin_src ")
                  srcLanguage [29882-29892] ("emacs-lisp")
              newLine [29892-29893]
              blockBody [29893-29921]
                text [29893-29921] ("(message \\"\\"\\\\[line [0-9]\\\\]\\"\\")")
              newLine [29921-29922]
              blockFooter [29922-29931]
                keyword [29922-29931]
                  text [29922-29931] ("#+end_src")
            newLine [29931-29932]
            srcBlock [29932-30067]
                :language emacs-lisp:
              blockHeader [29932-29954]
                keyword [29932-29954]
                    :language emacs-lisp:
                  text [29932-29944] ("#+begin_src ")
                  srcLanguage [29944-29954] ("emacs-lisp")
              newLine [29954-29955]
              blockBody [29955-30057]
                text [29955-30057] ("(message \\"%s\\" (string-match \\"{\\\\|);?$\\" \\"public replaceNonPrintableCharacters(text: string): string {\\"))")
              newLine [30057-30058]
              blockFooter [30058-30067]
                keyword [30058-30067]
                  text [30058-30067] ("#+end_src")
            newLine [30067-30068]
            keyword [30068-30079]
              text [30068-30079] ("#+RESULTS: ")
            newLine [30079-30080]
            fixedWidth [30080-30084]
              operator [30080-30082] (": ")
              text [30082-30084] ("59")
            newLine [30084-30085]
            newLine [30085-30086]
            newLine [30086-30087]
            srcBlock [30087-30268]
                :language emacs-lisp:
              blockHeader [30087-30109]
                keyword [30087-30109]
                    :language emacs-lisp:
                  text [30087-30099] ("#+begin_src ")
                  srcLanguage [30099-30109] ("emacs-lisp")
              newLine [30109-30110]
              blockBody [30110-30258]
                text [30110-30258] ("(setq turbo-log--ecmascript-final-symbols '(?; ?)))\\n(while (or (not (eobp)) (member ?) '(?; ?))))\\n                 (setq current-char char-after))))")
              newLine [30258-30259]
              blockFooter [30259-30268]
                keyword [30259-30268]
                  text [30259-30268] ("#+end_src")
            newLine [30268-30269]
            srcBlock [30269-31280]
                :language emacs-lisp:
              blockHeader [30269-30291]
                keyword [30269-30291]
                    :language emacs-lisp:
                  text [30269-30281] ("#+begin_src ")
                  srcLanguage [30281-30291] ("emacs-lisp")
              newLine [30291-30292]
              blockBody [30292-31270]
                text [30292-31270] ("(setq quicktype-mode-configs '((\\"go\\" go-mode \\"\\")\\n                               (\\"ts\\" typescript-mode \\"\\")\\n                               (\\"js\\" js2-mode \\"\\")\\n                               (\\"rs\\" rust-mode \\"\\")\\n                               (\\"c++\\" c++-mode \\"\\")\\n                               (\\"javascript-prop-types\\" js2-mode \\"\\")\\n                               (\\"flow\\" flow-js2-mode \\"\\")\\n                               (\\"swift\\" swift-mode \\"\\")\\n                               (\\"kotlin\\" kotlin-mode \\"\\")\\n                               (\\"elm\\" elm-mode \\"\\")\\n                               (\\"ruby\\" ruby-mode \\"\\")\\n                               (\\"dart\\" dart-mode \\"\\")\\n                               (\\"py\\" python-mode \\"--python-version 3.7\\")\\n                               (\\"haskell\\" haskell-mode \\"\\")))\\n\\n;; (message \\"%s\\" quicktype-mode-configs)\\n(message \\"%s\\" (cl-rassoc 'go-mode quicktype-mode-configs :test #'member))\\n;; (message \\"%s\\" (cl-rassoc \\"Red Pine\\" needles-per-cluster :test #'member))")
              newLine [31270-31271]
              blockFooter [31271-31280]
                keyword [31271-31280]
                  text [31271-31280] ("#+end_src")
            newLine [31280-31281]
            keyword [31281-31292]
              text [31281-31292] ("#+RESULTS: ")
            newLine [31292-31293]
            fixedWidth [31293-31308]
              operator [31293-31295] (": ")
              text [31295-31308] ("(go go-mode )")
            newLine [31308-31309]
            newLine [31309-31310]
            newLine [31310-31311]
            srcBlock [31311-31534]
                :language emacs-lisp:
              blockHeader [31311-31333]
                keyword [31311-31333]
                    :language emacs-lisp:
                  text [31311-31323] ("#+begin_src ")
                  srcLanguage [31323-31333] ("emacs-lisp")
              newLine [31333-31334]
              blockBody [31334-31524]
                text [31334-31524] ("(setq needles-per-cluster\\n      '((2 \\"Austrian Pine\\" \\"Red Pine\\")\\n        (3 \\"Pitch Pine\\")\\n        (5 \\"White Pine\\")))\\n\\n(message \\"%s\\" (cl-rassoc \\"Red Pine\\" needles-per-cluster :test #'member))")
              newLine [31524-31525]
              blockFooter [31525-31534]
                keyword [31525-31534]
                  text [31525-31534] ("#+end_src")
            newLine [31534-31535]
            keyword [31535-31546]
              text [31535-31546] ("#+RESULTS: ")
            newLine [31546-31547]
            fixedWidth [31547-31575]
              operator [31547-31549] (": ")
              text [31549-31575] ("(2 Austrian Pine Red Pine)")
            newLine [31575-31576]
            newLine [31576-31577]
            newLine [31577-31578]
            srcBlock [31578-31714]
                :language emacs-lisp:
              blockHeader [31578-31600]
                keyword [31578-31600]
                    :language emacs-lisp:
                  text [31578-31590] ("#+begin_src ")
                  srcLanguage [31590-31600] ("emacs-lisp")
              newLine [31600-31601]
              blockBody [31601-31704]
                text [31601-31704] ("(message \\"%s\\" (string-match \\"\\\\({\\\\|;$\\\\)\\\\|\\\\(const [\\\\w\\\\[:digit]]+ = [\\\\d[:digit:]]+$\\\\)\\" \\"  const foo = 1\\"))")
              newLine [31704-31705]
              blockFooter [31705-31714]
                keyword [31705-31714]
                  text [31705-31714] ("#+end_src")
            newLine [31714-31715]
            keyword [31715-31726]
              text [31715-31726] ("#+RESULTS: ")
            newLine [31726-31727]
            fixedWidth [31727-31732]
              operator [31727-31729] (": ")
              text [31729-31732] ("nil")
            newLine [31732-31733]
      "
    `);
  });

  it('Should parse complext org node with scss description', () => {
    const orgDoc = `:PROPERTIES:
:ID: scss
:END:

#+TITLE: Верстка.
#+DESCRIPTION: Подборка всякого для верстки.
#+FILETAGS: :scss:css:sass:вестка:
#+ID: scss
#+ACTIVE:


* Ссылки
** Видео
*** [[https://www.youtube.com/watch?v=nOdDtnHWaDo][Кубы css анимация]]
** Design tokens
**** [[https://medium.com/@uxlord/what-the-are-design-tokens-2020-f3c4f1258349][Коротко о проблемах]]
**** [[https://uxdesign.cc/design-tokens-cheatsheet-927fc1404099#:~:text=Variables%20%E2%89%A0%20Design%20Tokens&text=Design%20Tokens%20are%20used%20in,with%20developers%20using%20these%20terms.][Типы дизайн токенов]]
**** [[https://youtu.be/M0iZg7mlCEE][Практическое применение токенов]]
**** [[https://www.figma.com/community/plugin/843461159747178978/Figma-Tokens][Плагин]]
**** [[https://docs.tokens.studio/][Его документация]]
**** [[https://www.youtube.com/watch?v=Ka1I5TphDb0][Видео про плагин]] (eng)
**** [[https://amzn.github.io/style-dictionary/#/][Style dictionary, конвертация design tokens в sass]]
* Css
** Outline
#+BEGIN_SRC scss
textarea:focus, input:focus{
    outline: none;
}
#+END_SRC
** Hide scrollbar
:PROPERTIES:
:ID: css-hide-scrollbar
:END:
#+BEGIN_SRC css
.example::-webkit-scrollbar {
  display: none;
}
#+END_SRC
** Многоточие в конце мультистрочного текста:
#+BEGIN_SRC scss
.example {
  overflow: hidden;
  display: -webkit-box;
  -webkit-line-clamp: 3;
  -webkit-box-orient: vertical;
}

* Миксины
** Mixin для media queries
[[https://rimdev.io/making-media-query-mixins-with-sass/][Подробнее тут]]
#+BEGIN_SRC scss
@mixin breakpoint($breakpoint, $direction) {
  @if map-has-key($breakpoints, $breakpoint) {

    // Get the breakpoint value.
    $breakpoint-value: map-get($breakpoints, $breakpoint);

    @if $direction == max {
      @media (max-width: ($breakpoint-value - 1)) {
        @content;
      }
    } @else if $direction == min {
      @media (min-width: $breakpoint-value) {
        @content;
      }
    }

  // If the breakpoint doesn't exist in the map.
  } @else {
    @if $direction == max {
      @media (max-width: $breakpoint) {
        @content;
      }
    } @else if $direction == min {
      @media (min-width: $breakpoint) {
        @content;
      }
    }
  }
}

#+END_SRC`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-2192]
          :id  scss:
        propertyDrawer [0-28]
          property [0-12]
            text [0-12] (":PROPERTIES:")
          newLine [12-13]
          property [13-22]
            text [13-18] (":ID: ")
            text [18-22] ("scss")
          newLine [22-23]
          property [23-28]
            text [23-28] (":END:")
        newLine [28-29]
        newLine [29-30]
        keyword [30-47]
          text [30-39] ("#+TITLE: ")
          text [39-47] ("Верстка.")
        newLine [47-48]
        keyword [48-92]
          text [48-63] ("#+DESCRIPTION: ")
          text [63-92] ("Подборка всякого для верстки.")
        newLine [92-93]
        keyword [93-127]
          text [93-105] ("#+FILETAGS: ")
          tagList [105-127]
            operator [105-106] (":")
            text [106-110] ("scss")
            operator [110-111] (":")
            text [111-114] ("css")
            operator [114-115] (":")
            text [115-119] ("sass")
            operator [119-120] (":")
            text [120-126] ("вестка")
            operator [126-127] (":")
        newLine [127-128]
        keyword [128-138]
          text [128-134] ("#+ID: ")
          text [134-138] ("scss")
        newLine [138-139]
        keyword [139-148]
          text [139-148] ("#+ACTIVE:")
        newLine [148-149]
        newLine [149-150]
        newLine [150-151]
        headline [151-972]
            :level 1:
          title [151-160]
            operator [151-153] ("* ")
            text [153-159] ("Ссылки")
            newLine [159-160]
          section [160-972]
            headline [160-240]
                :level 2:
              title [160-169]
                operator [160-163] ("** ")
                text [163-168] ("Видео")
                newLine [168-169]
              section [169-240]
                headline [169-240]
                    :level 3:
                  title [169-240]
                    operator [169-173] ("*** ")
                    link [173-239]
                        :linkType network:
                      operator [173-174] ("[")
                      linkUrl [174-219]
                        operator [174-175] ("[")
                        text [175-218] ("https://www.youtube.com/watch?v=nOdDtnHWaDo")
                        operator [218-219] ("]")
                      linkName [219-238]
                        operator [219-220] ("[")
                        text [220-237] ("Кубы css анимация")
                        operator [237-238] ("]")
                      operator [238-239] ("]")
                    newLine [239-240]
                  section [240-240]
            headline [240-972]
                :level 2:
              title [240-257]
                operator [240-243] ("** ")
                text [243-256] ("Design tokens")
                newLine [256-257]
              section [257-972]
                headline [257-359]
                    :level 4:
                  title [257-359]
                    operator [257-262] ("**** ")
                    link [262-358]
                        :linkType network:
                      operator [262-263] ("[")
                      linkUrl [263-336]
                        operator [263-264] ("[")
                        text [264-335] ("https://medium.com/@uxlord/what-the-are-design-tokens-2020-f3c4f1258349")
                        operator [335-336] ("]")
                      linkName [336-357]
                        operator [336-337] ("[")
                        text [337-356] ("Коротко о проблемах")
                        operator [356-357] ("]")
                      operator [357-358] ("]")
                    newLine [358-359]
                  section [359-359]
                headline [359-577]
                    :level 4:
                  title [359-577]
                    operator [359-364] ("**** ")
                    link [364-576]
                        :linkType network:
                      operator [364-365] ("[")
                      linkUrl [365-554]
                        operator [365-366] ("[")
                        text [366-553] ("https://uxdesign.cc/design-tokens-cheatsheet-927fc1404099#:~:text=Variables%20%E2%89%A0%20Design%20Tokens&text=Design%20Tokens%20are%20used%20in,with%20developers%20using%20these%20terms.")
                        operator [553-554] ("]")
                      linkName [554-575]
                        operator [554-555] ("[")
                        text [555-574] ("Типы дизайн токенов")
                        operator [574-575] ("]")
                      operator [575-576] ("]")
                    newLine [576-577]
                  section [577-577]
                headline [577-648]
                    :level 4:
                  title [577-648]
                    operator [577-582] ("**** ")
                    link [582-647]
                        :linkType network:
                      operator [582-583] ("[")
                      linkUrl [583-613]
                        operator [583-584] ("[")
                        text [584-612] ("https://youtu.be/M0iZg7mlCEE")
                        operator [612-613] ("]")
                      linkName [613-646]
                        operator [613-614] ("[")
                        text [614-645] ("Практическое применение токенов")
                        operator [645-646] ("]")
                      operator [646-647] ("]")
                    newLine [647-648]
                  section [648-648]
                headline [648-736]
                    :level 4:
                  title [648-736]
                    operator [648-653] ("**** ")
                    link [653-735]
                        :linkType network:
                      operator [653-654] ("[")
                      linkUrl [654-726]
                        operator [654-655] ("[")
                        text [655-725] ("https://www.figma.com/community/plugin/843461159747178978/Figma-Tokens")
                        operator [725-726] ("]")
                      linkName [726-734]
                        operator [726-727] ("[")
                        text [727-733] ("Плагин")
                        operator [733-734] ("]")
                      operator [734-735] ("]")
                    newLine [735-736]
                  section [736-736]
                headline [736-791]
                    :level 4:
                  title [736-791]
                    operator [736-741] ("**** ")
                    link [741-790]
                        :linkType network:
                      operator [741-742] ("[")
                      linkUrl [742-771]
                        operator [742-743] ("[")
                        text [743-770] ("https://docs.tokens.studio/")
                        operator [770-771] ("]")
                      linkName [771-789]
                        operator [771-772] ("[")
                        text [772-788] ("Его документация")
                        operator [788-789] ("]")
                      operator [789-790] ("]")
                    newLine [790-791]
                  section [791-791]
                headline [791-868]
                    :level 4:
                  title [791-868]
                    operator [791-796] ("**** ")
                    link [796-861]
                        :linkType network:
                      operator [796-797] ("[")
                      linkUrl [797-842]
                        operator [797-798] ("[")
                        text [798-841] ("https://www.youtube.com/watch?v=Ka1I5TphDb0")
                        operator [841-842] ("]")
                      linkName [842-860]
                        operator [842-843] ("[")
                        text [843-859] ("Видео про плагин")
                        operator [859-860] ("]")
                      operator [860-861] ("]")
                    text [861-867] (" (eng)")
                    newLine [867-868]
                  section [868-868]
                headline [868-972]
                    :level 4:
                  title [868-972]
                    operator [868-873] ("**** ")
                    link [873-971]
                        :linkType network:
                      operator [873-874] ("[")
                      linkUrl [874-918]
                        operator [874-875] ("[")
                        text [875-917] ("https://amzn.github.io/style-dictionary/#/")
                        operator [917-918] ("]")
                      linkName [918-970]
                        operator [918-919] ("[")
                        text [919-969] ("Style dictionary, конвертация design tokens в sass")
                        operator [969-970] ("]")
                      operator [970-971] ("]")
                    newLine [971-972]
                  section [972-972]
        headline [972-1380]
            :level 1:
          title [972-978]
            operator [972-974] ("* ")
            text [974-977] ("Css")
            newLine [977-978]
          section [978-1380]
            headline [978-1066]
                :level 2:
              title [978-989]
                operator [978-981] ("** ")
                text [981-988] ("Outline")
                newLine [988-989]
              section [989-1066]
                srcBlock [989-1065]
                    :language scss:
                  blockHeader [989-1005]
                    keyword [989-1005]
                        :language scss:
                      text [989-1001] ("#+BEGIN_SRC ")
                      srcLanguage [1001-1005] ("scss")
                  newLine [1005-1006]
                  blockBody [1006-1055]
                    text [1006-1055] ("textarea:focus, input:focus{\\n    outline: none;\\n}")
                  newLine [1055-1056]
                  blockFooter [1056-1065]
                    keyword [1056-1065]
                      text [1056-1065] ("#+END_SRC")
                newLine [1065-1066]
            headline [1066-1202]
                :level 2:
                :id  css-hide-scrollbar:
              title [1066-1084]
                operator [1066-1069] ("** ")
                text [1069-1083] ("Hide scrollbar")
                newLine [1083-1084]
              section [1084-1202]
                propertyDrawer [1084-1126]
                  property [1084-1096]
                    text [1084-1096] (":PROPERTIES:")
                  newLine [1096-1097]
                  property [1097-1120]
                    text [1097-1102] (":ID: ")
                    text [1102-1120] ("css-hide-scrollbar")
                  newLine [1120-1121]
                  property [1121-1126]
                    text [1121-1126] (":END:")
                newLine [1126-1127]
                srcBlock [1127-1201]
                    :language css:
                  blockHeader [1127-1142]
                    keyword [1127-1142]
                        :language css:
                      text [1127-1139] ("#+BEGIN_SRC ")
                      srcLanguage [1139-1142] ("css")
                  newLine [1142-1143]
                  blockBody [1143-1191]
                    text [1143-1191] (".example::-webkit-scrollbar {\\n  display: none;\\n}")
                  newLine [1191-1192]
                  blockFooter [1192-1201]
                    keyword [1192-1201]
                      text [1192-1201] ("#+END_SRC")
                newLine [1201-1202]
            headline [1202-1380]
                :level 2:
              title [1202-1248]
                operator [1202-1205] ("** ")
                text [1205-1247] ("Многоточие в конце мультистрочного текста:")
                newLine [1247-1248]
              section [1248-1380]
                keyword [1248-1264]
                  text [1248-1260] ("#+BEGIN_SRC ")
                  text [1260-1264] ("scss")
                newLine [1264-1265]
                text [1265-1274] (".example ")
                text [1274-1275] ("{")
                newLine [1275-1276]
                indent [1276-1278] ("  ")
                text [1278-1295] ("overflow: hidden;")
                newLine [1295-1296]
                indent [1296-1298] ("  ")
                text [1298-1319] ("display: -webkit-box;")
                newLine [1319-1320]
                indent [1320-1322] ("  ")
                text [1322-1344] ("-webkit-line-clamp: 3;")
                newLine [1344-1345]
                indent [1345-1347] ("  ")
                text [1347-1376] ("-webkit-box-orient: vertical;")
                newLine [1376-1377]
                text [1377-1378] ("}")
                newLine [1378-1379]
                newLine [1379-1380]
        headline [1380-2192]
            :level 1:
          title [1380-1390]
            operator [1380-1382] ("* ")
            text [1382-1389] ("Миксины")
            newLine [1389-1390]
          section [1390-2192]
            headline [1390-2192]
                :level 2:
              title [1390-1417]
                operator [1390-1393] ("** ")
                text [1393-1416] ("Mixin для media queries")
                newLine [1416-1417]
              section [1417-2192]
                link [1417-1490]
                    :linkType network:
                  operator [1417-1418] ("[")
                  linkUrl [1418-1474]
                    operator [1418-1419] ("[")
                    text [1419-1473] ("https://rimdev.io/making-media-query-mixins-with-sass/")
                    operator [1473-1474] ("]")
                  linkName [1474-1489]
                    operator [1474-1475] ("[")
                    text [1475-1488] ("Подробнее тут")
                    operator [1488-1489] ("]")
                  operator [1489-1490] ("]")
                newLine [1490-1491]
                srcBlock [1491-2192]
                    :language scss:
                  blockHeader [1491-1507]
                    keyword [1491-1507]
                        :language scss:
                      text [1491-1503] ("#+BEGIN_SRC ")
                      srcLanguage [1503-1507] ("scss")
                  newLine [1507-1508]
                  blockBody [1508-2182]
                    text [1508-2182] ("@mixin breakpoint($breakpoint, $direction) {\\n  @if map-has-key($breakpoints, $breakpoint) {\\n\\n    // Get the breakpoint value.\\n    $breakpoint-value: map-get($breakpoints, $breakpoint);\\n\\n    @if $direction == max {\\n      @media (max-width: ($breakpoint-value - 1)) {\\n        @content;\\n      }\\n    } @else if $direction == min {\\n      @media (min-width: $breakpoint-value) {\\n        @content;\\n      }\\n    }\\n\\n  // If the breakpoint doesn't exist in the map.\\n  } @else {\\n    @if $direction == max {\\n      @media (max-width: $breakpoint) {\\n        @content;\\n      }\\n    } @else if $direction == min {\\n      @media (min-width: $breakpoint) {\\n        @content;\\n      }\\n    }\\n  }\\n}\\n")
                  newLine [2182-2183]
                  blockFooter [2183-2192]
                    keyword [2183-2192]
                      text [2183-2192] ("#+END_SRC")
      "
    `);
  });

  it('Should parse complex file from real world', () => {
    const orgDoc = `| +      | 2  |`;

    const result = parse(orgDoc);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-15]
        table [0-15]
          tableRow [0-15]
            operator [0-1] ("|")
            tableCell [1-9]
              text [1-9] (" +      ")
            operator [9-10] ("|")
            tableCell [10-14]
              text [10-14] (" 2  ")
            operator [14-15] ("|")
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  it('Should parse complex example from real world', () => {
    const orgDoc = `
#+begin_src python
return [f'{*}', '*']
#+end_src

`;
    const result = parse(orgDoc);
    console.log('✎: [line 5174][complex.spec.ts] result: ', result.toString());
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-52]
        newLine [0-1]
        srcBlock [1-50]
            :language python:
          blockHeader [1-19]
            keyword [1-19]
                :language python:
              text [1-13] ("#+begin_src ")
              srcLanguage [13-19] ("python")
          newLine [19-20]
          blockBody [20-40]
            text [20-40] ("return [f'{*}', '*']")
          newLine [40-41]
          blockFooter [41-50]
            keyword [41-50]
              text [41-50] ("#+end_src")
        newLine [50-51]
        newLine [51-52]
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  it('Should correct parse string with multiple nested bracket operators', () => {
    const orgDoc = `[f'{*}', '*']`;
    const result = parse(orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-13]
        text [0-13] ("[f'{*}', '*']")
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  it('Should parse complex example from the real world! 2', () => {
    const orgDoc = `**** Примеры
#+BEGIN_SRC yaml
affinity:
  - matchExpression:
 #+END_SRC

 #+BEGIN_SRC yaml
affinity:
      - weight: 1
#+END_SRC
**** Пример
`;
    const result = parse(orgDoc);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-141]
        headline [0-129]
            :level 4:
          title [0-13]
            operator [0-5] ("**** ")
            text [5-12] ("Примеры")
            newLine [12-13]
          section [13-129]
            srcBlock [13-71]
                :language yaml:
              blockHeader [13-29]
                keyword [13-29]
                    :language yaml:
                  text [13-25] ("#+BEGIN_SRC ")
                  srcLanguage [25-29] ("yaml")
              newLine [29-30]
              blockBody [30-62]
                text [30-62] ("affinity:\\n  - matchExpression:\\n ")
              blockFooter [62-71]
                keyword [62-71]
                  text [62-71] ("#+END_SRC")
            newLine [71-72]
            newLine [72-73]
            srcBlock [73-128]
                :language yaml:
              blockHeader [73-90]
                indent [73-74] (" ")
                keyword [74-90]
                    :language yaml:
                  text [74-86] ("#+BEGIN_SRC ")
                  srcLanguage [86-90] ("yaml")
              newLine [90-91]
              blockBody [91-119]
                text [91-119] ("affinity:\\n      - weight: 1\\n")
              blockFooter [119-128]
                keyword [119-128]
                  text [119-128] ("#+END_SRC")
            newLine [128-129]
        headline [129-141]
            :level 4:
          title [129-141]
            operator [129-134] ("**** ")
            text [134-140] ("Пример")
            newLine [140-141]
          section [141-141]
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  xit('Should parse complex example from the real world! 3', () => {
    const orgDoc = `:PROPERTIES:
:ID: kubernetes-settings
:END:

#+TITLE: Kubernetes. Использование в продакшене.
#+DESCRIPTION: Введение, примеры настроек, терминология.
#+FILETAGS: :kubernetes:кубернетис:оркестратор:k8s:
#+STARTUP: content
#+STARTUP: hideblocks
#+ID: kubernetes-settings
#+ACTIVE:



Кубернетис - популярный [[https://ru.wikipedia.org/wiki/%D0%9E%D1%80%D0%BA%D0%B5%D1%81%D1%82%D1%80%D0%BE%D0%B2%D0%BA%D0%B0_(%D0%98%D0%A2)][оркестратор]] контейнеров, также известный как k8s/kube

*Ликбез*:
+ Управлять множеством серверов как одним
+ Сделан гуглом
+ По умолчанию использует докер для запуска контейнеров (однао поддерживает другие механизмы - runtimes. Такие как containerd, CRI-O)
+ Предоставляет набор доступов через API/CLI
+ Поставляется в большинстве облачных решений :)
+ Позволяет быстро масштабировать реальные физические сервера
+ Позволяет мониторить состояние узлов
+ Может быть избыточным при малом количестве изменений, либо маленьких командах
* Ссылки
:PROPERTIES:
:ID: kubernetes-resources
:END:
+ [[https://k8slens.dev/][IDE для работы с кубером]]
+ [[https://habr.com/ru/company/otus/blog/537162/][Азы]]
+ [[id:kuber][Kubernetes установка]]
+ Для изучения понадобиться [[https://docs.docker.com/desktop/mac/install/][docker for mac]] или [[https://kubernetes.io/ru/docs/tasks/tools/install-minikube/][minikube]]
+ [[https://labs.play-with-k8s.com/][k8s playground]]
+ [[https://www.katacoda.com/][Еще 1 песочница для изучения cloud технологий в т.ч. k8s]]
+ [[https://kubernetes.io/ru/docs/reference/kubectl/cheatsheet/][Шпаргалка для работы с кубером]]
+ [[https://lionelmace.github.io/iks-lab/gitlab-registry.html][Gitlab подключение registry]]
+ [[https://habr.com/ru/company/domclick/blog/577964/][Полный гайд по куберу + cd на голом железе]] (хабр)
+ [[https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/kubectl][zsh plugin для kubernetes]]
+ [[https://ealebed.github.io/posts/2018/%D0%B7%D0%BD%D0%B0%D0%BA%D0%BE%D0%BC%D1%81%D1%82%D0%B2%D0%BE-%D1%81-kubernetes-%D1%87%D0%B0%D1%81%D1%82%D1%8C-16-%D1%80%D0%B0%D0%B7%D0%BB%D0%B8%D1%87%D0%B8%D1%8F-%D0%B2-replication-controller-replica-set-%D0%B8-deployments/][Отличие Replication Controller от Replica Set и Deployments]]
+ [[https://leadwithoutatitle.wordpress.com/2018/03/05/how-to-deploy-mongodb-with-persistent-volume-in-kubernetes/][Kubernetes deploy mongodb]]
+ [[https://github.com/kubernetes/examples/blob/master/staging/nodesjs-mongodb/README.md][Mongo db + nodejs пример]]
+ [[https://www.digitalocean.com/community/tutorials/how-to-set-up-an-nginx-ingress-with-cert-manager-on-digitalocean-kubernetes-ru][Настройка nginx ingress с cert-manager]]
+ [[https://www.youtube.com/watch?v=LLVfC08UVqY&t=73s][Курс по кубернетису от Слерм (youtube, ru)]]
  [[https://github.com/Slurmio/school-dev-k8s][Github с уроками и примерами ребят сверху]]
+ [[https://medium.com/@thms.hmm/docker-for-mac-with-kubernetes-ingress-controller-with-traefik-e194919591bb][Ingress, docker for mac]]
+ [[https://itnext.io/goodbye-docker-desktop-hello-minikube-3649f2a1c469][Установка minicube mac os]]
+ [[https://medium.com/@seohee.sophie.kwon/how-to-run-a-minikube-on-apple-silicon-m1-8373c248d669][Minicube macos m1 (arm)]]


** Внешние обучающие ресурсы:noexport:
+ https://github.com/bretfisher/udemy-docker-mastery
+ https://www.youtube.com/watch?v=V6aGfrMXhbA&list=PL8D2P0ruohOBSA_CDqJLflJ8FLJNe26K-&index=2
** ПО для облегчения работы
+ [[https://k9scli.io/][k9s cli утилита для работы с кубером]]
* Терминология
:PROPERTIES:
:ID: kubernetes-terms
:END:
*kubectl* - [[id:cli][cli]] (Command line tool) для управления кубером и приложениями.

*Control plane* - набор контейнеров, которые управляют кластером (аналог менеджеров в [[id:docker-swarm][Docker swarm]], по факту - набор мастер узлов)
*Master node* Мастер узел состоит из:

+ *etcd* - key/value storage
+ API
+ scheduler
+ Controller manager
+ Core DNS - управление ns записями.

C помощью мастер-узла происходит управление всем кластером Kubernetes.

На *Worker node* находятся container runtime (среда запуска контейнера), kubelet и kube-proxy.

Сontainer runtime это то на чем будет запущен ваш Под (например Docker, Container D, Rocket и т.д.).

*Kubelet* это основной «агент узла», который работает на каждой ноде. Гарантирует, что контейнеры в Pod(поде)работают и исправны. Не управляет контейнерами, которые не были созданы Kubernetes.

*Kube-proxy* это демон на каждой ноде, управляет правилами iptable на хосте для достижения балансировки нагрузки службы (одна из реализаций) и следит за изменениями Service и Endpoint.


*Pod* (под) - это группа из одного или более контейнера с общим хранилищем/сетевыми ресурсами и спецификацией как запускать контейнеры. Так же это отдельный инстанс приложения. Размещая контейнеры таким образом, Kubernetes устраняет соблазн втиснуть слишком много функций в один образ контейнера.


*Controller* - управленец над созданием и обновлением подов.

Концепция *Service (Сервисы)* в Kubernetes используется для группирования нескольких подов, которые выполняют те же функции. Сервисы легко настраиваются для таких целей как обнаружение, горизонтальное масштабирование и балансировка нагрузки.

*Namespace* - фильтр для cli

*Replication Controller* - Уровень абстракции позволяющий создавать несколько экземпляров подов для последующей балансировки нагрузки и повышения отказоустойчивости.

*Replication Set* - тоже самое что и Replication Controller но с поддержкой множественного выбора в селектора, кроме того не поддерживает Rolling Update. По большому счету, replication set это темплейт для подов.

*Deployment* - еще более высокий уровень асбтракции, поддерживает rolling-update/rollback а также множественный выбор в селекторе.

*Stateful Set* - Тоже самое что и deployment, однако позволяет указывать порядок запуска подов, а также иметь persistence volume (пока понятно так ли это)?

* Kubernetes vs swarm
+ Swarm и kubernetes - оркестраторы контейнеров
+ Оба из них - надежные платформы с поддержкой поставщиков
+ Swarm - проще в управлении, добавлении и удалении узлов
+ Kubernetes решает более сложные задачи

  *Преимущеста swarm*
  + Поставляется с Docker
  + Прост в управлении
  + Решает основные задачи, можно сравнить с правилом 20 на 80, в этом случае swarm покрывает около 80 процентов потребностей
  + Запускается на мноежстве платформ, ARM/windows/32-bit как локально так и на удаленной машине, либо облаке
  + Реализует механизмы безопасностb из коробки (secure secrets)
  + Позволяет легко решать возникшие проблемы (благодаря хорошему логированию и комьюнити)

  *Преимущества kubernetes*
  + Большие облака будут развертывать кубернетис и управлять им за вас.
  + Множество дистрибутивов от разных популярных компаний
  + Широкая поддержка комьюнити
  + Гибкость, огромное количество виджетов и наборов для решения различных юзкейсов
  + При разработке новых продуктов для инфраструктуры, производитель в 1 очередь учитывает возможность интеграции именно с kubernetes

* Quick Start
Для начала подключим кубер внутри docker for mac (либо установим minikube)
#+attr_html: :width 100%
[[./enable-kuber.jpg]]

** Основные команды
Представлена лишь малая часть. Бльшинство операций можно выполнить разными способами.

*** kubectl run
Изменения только для создания подов
*** kubectl create
Создает ресурсы с помощью CLI или YAML
*** kubectl apply
Создает/обновляет с помощью YAML
*** kubectl version
#+START_SPOILER Посмотреть версию кубера: >
#+BEGIN_SRC bash
❯ kubectl version
Client Version: version.Info{Major:"1", Minor:"21", GitVersion:"v1.21.3", GitCommit:"ca643a4d1f7bfe34773c74f79527be4afd95bf39", GitTreeState:"clean", BuildDate:"2021-07-15T21:04:39Z", GoVersion:"go1.16.6", Compiler:"gc", Platform:"darwin/amd64"}
Server Version: version.Info{Major:"1", Minor:"20", GitVersion:"v1.20.7", GitCommit:"b55a30656180655e4773309bc68268b87394142f", GitTreeState:"clean", BuildDate:"2021-11-01T16:42:19Z", GoVersion:"go1.15.12", Compiler:"gc", Platform:"linux/amd64"}
#+END_SRC
#+CLOSE_SPOILER
** Создание пода:
*** CLI
#+BEGIN_SRC bash
❯ kubectl run my-nginx --image nginx
pod/my-nginx created
#+END_SRC


Посмотреть доступные поды
#+BEGIN_SRC bash
❯ kubectl get pods
NAME       READY   STATUS    RESTARTS   AGE
my-nginx   1/1     Running   0          15s
#+END_SRC


Посомтреть все объекты:
#+BEGIN_SRC bash
❯ kubectl get all
NAME           READY   STATUS    RESTARTS   AGE
pod/my-nginx   1/1     Running   0          86s

NAME                   TYPE        CLUSTER-IP    EXTERNAL-IP   PORT(S)    AGE
service/kubernetes     ClusterIP   10.0.0.1      <none>        443/TCP    102d
service/oauth2-proxy   ClusterIP   10.0.29.165   <none>        4180/TCP   97d
#+END_SRC

Очистим созданное:
#+BEGIN_SRC bash
❯ kubectl delete pod my-nginx
pod "my-nginx" deleted
#+END_SRC
** Создание deployment
В отличии от pod, deployment это рабочее приложение в кластере
#+BEGIN_SRC bash
❯ kubectl create deployment nginx --image nginx
deployment.apps/nginx created
#+END_SRC

#+BEGIN_SRC bash
❯ kubectl get all
NAME                         READY   STATUS    RESTARTS   AGE
pod/nginx-6799fc88d8-ck2r2   1/1     Running   0          34s

NAME                   TYPE        CLUSTER-IP    EXTERNAL-IP   PORT(S)    AGE
service/kubernetes     ClusterIP   10.0.0.1      <none>        443/TCP    102d
service/oauth2-proxy   ClusterIP   10.0.29.165   <none>        4180/TCP   97d

NAME                    READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/nginx   1/1     1            1           35s

NAME                               DESIRED   CURRENT   READY   AGE
replicaset.apps/nginx-6799fc88d8   1         1         1       35s
#+END_SRC

Объектов создалось больше чем ожидалось :) Вокруг пода создается ReplicaseSet и Deployment

Очистим созданное
#+BEGIN_SRC bash
❯ kubectl delete deployment nginx
deployment.apps "nginx" deleted
#+END_SRC
** Создание реплик
Для начала создадим
#+BEGIN_SRC bash
❯ kubectl create deployment my-apache --image httpd
deployment.apps/my-apache created
❯ kubectl get all
NAME                             READY   STATUS    RESTARTS   AGE
pod/my-apache-7b68fdd849-p6d65   1/1     Running   0          9s

NAME                   TYPE        CLUSTER-IP    EXTERNAL-IP   PORT(S)    AGE
service/kubernetes     ClusterIP   10.0.0.1      <none>        443/TCP    102d
service/oauth2-proxy   ClusterIP   10.0.29.165   <none>        4180/TCP   97d

NAME                        READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/my-apache   1/1     1            1           10s

NAME                                   DESIRED   CURRENT   READY   AGE
replicaset.apps/my-apache-7b68fdd849   1         1         1       10s
#+END_SRC

Увеличим число реплик
#+BEGIN_SRC bash
❯ kubectl scale deploy/my-apache --replicas 2
#+END_SRC

(альтернативно можно и так запустить =kubectl scale deployment my-apache --replicas 2=)
#+START_SPOILER Теперь у нас 2 реплики: >
#+BEGIN_SRC bash
❯ kubectl get all
NAME                             READY   STATUS    RESTARTS   AGE
pod/my-apache-7b68fdd849-4z7fm   1/1     Running   0          6m45s
pod/my-apache-7b68fdd849-p6d65   1/1     Running   0          9m56s

NAME                   TYPE        CLUSTER-IP    EXTERNAL-IP   PORT(S)    AGE
service/kubernetes     ClusterIP   10.0.0.1      <none>        443/TCP    102d
service/oauth2-proxy   ClusterIP   10.0.29.165   <none>        4180/TCP   97d

NAME                        READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/my-apache   2/2     2            2           9m56s

NAME                                   DESIRED   CURRENT   READY   AGE
replicaset.apps/my-apache-7b68fdd849   2         2         2       9m56s
#+END_SRC
#+CLOSE_SPOILER
** Инспеция deployment 🕵🏻‍♀️
*** Логи
Посмотреть логик =kubectl logs deployment <name>

#+START_SPOILER Вывод логов >
#+BEGIN_SRC bash
❯ kubectl logs deployment/my-apache
Found 2 pods, using pod/my-apache-7b68fdd849-p6d65
AH00558: httpd: Could not reliably determine the server's fully qualified domain name, using 10.244.9.136. Set the 'ServerName' directive globally to suppress this message
         AH00558: httpd: Could not reliably determine the server's fully qualified domain name, using 10.244.9.136. Set the 'ServerName' directive globally to suppress this message
[Sun Dec 12 22:03:43.199360 2021] [mpm_event:notice] [pid 1:tid 140063025028416] AH00489: Apache/2.4.51 (Unix) configured -- resuming normal operations
[Sun Dec 12 22:03:43.199507 2021] [core:notice] [pid 1:tid 140063025028416] AH00094: Command line: 'httpd -D FOREGROUND'
#+END_SRC
#+CLOSE_SPOILER

Логер, также как и логер в докере (и других юниксовых утилитах) поддерживает
=--tail <N>=для вывода последних N строк и
=--follow= для просмотра
=-l= - лейбл
*** Describe
#+BEGIN_SRC bash
❯ kubectl get pods
NAME                         READY   STATUS    RESTARTS   AGE
my-apache-7b68fdd849-4z7fm   1/1     Running   0          26m
my-apache-7b68fdd849-p6d65   1/1     Running   0          30m
#+END_SRC

#+START_SPOILER Очень большое описание нашего пода >
#+BEGIN_SRC bash
❯ kubectl describe pod my-apache-7b68fdd849-p6d65
Name:         my-apache-7b68fdd849-p6d65
Namespace:    default
Priority:     0
Node:         aks-verifika-53237813-vmss00000c/10.240.0.6
Start Time:   Mon, 13 Dec 2021 01:03:36 +0300
Labels:       app=my-apache
pod-template-hash=7b68fdd849
Annotations:  <none>
Status:       Running
IP:           10.244.9.136
IPs:
IP:           10.244.9.136
Controlled By:  ReplicaSet/my-apache-7b68fdd849
Containers:
httpd:
Container ID:   containerd://e5fa5c4be01456e18bb5eb1b9b7d12edbe52d4c295b50a5d07cfbeb364aee153
Image:          httpd
Image ID:       docker.io/library/httpd@sha256:fba8a9f4290180ceee5c74638bb85ff21fd15961e6fdfa4def48e18820512bb1
Port:           <none>
Host Port:      <none>
State:          Running
Started:      Mon, 13 Dec 2021 01:03:43 +0300
Ready:          True
Restart Count:  0
Environment:    <none>
Mounts:
/var/run/secrets/kubernetes.io/serviceaccount from default-token-4rt8v (ro)
Conditions:
Type              Status
Initialized       True
Ready             True
ContainersReady   True
PodScheduled      True
Volumes:
default-token-4rt8v:
Type:        Secret (a volume populated by a Secret)
SecretName:  default-token-4rt8v
Optional:    false
QoS Class:       BestEffort
Node-Selectors:  <none>
Tolerations:     node.kubernetes.io/not-ready:NoExecute op=Exists for 300s
node.kubernetes.io/unreachable:NoExecute op=Exists for 300s
Events:
Type    Reason     Age   From               Message
----    ------     ----  ----               -------
Normal  Scheduled  31m   default-scheduler  Successfully assigned default/my-apache-7b68fdd849-p6d65 to aks-verifika-53237813-vmss00000c
Normal  Pulling    31m   kubelet            Pulling image "httpd"
Normal  Pulled     31m   kubelet            Successfully pulled image "httpd" in 4.330055703s
Normal  Created    31m   kubelet            Created container httpd
Normal  Started    31m   kubelet            Started container httpd
#+END_SRC

#+CLOSE_SPOILER
** Services
:PROPERTIES:
:ID: kubernetes services
:END:
 Service - это стабильный аддресс для пода(ов)
=kubectl expose= - создает сервис
=CoreDNS= - позволяет перенаправлять трафик на сервисы по имени
*** Основные типы сервисов
+ ClusterIP
+ NodePort
+ LoadBalancer
+ ExternalName

**** Cluster ip (default)
- Доступен только внутри кластера!
- Имеет внутренний виртуальный IP внутри кластера.
- Поды могут стучаться по определенному порту
**** NodePort
- Необходим для чего-то внешнего, может быть достигнут кем угодно снаружи
- Доступен на каждом узле
# TODO: разобраться так ли это
**** LoadBalancer
- Контролируется через ендпоинты снаружи кластера (?)
- Доступен только когда внешний провайдер предоставляет такую возможность (AWS ELB, etc)
**** External name
- Добавялет CNAME DNS запись в CoreDNS /Хз что это/
- Не используется для подов, но дает подам DNS имя для использования где-то снаружи кубера
*** Создание
#+BEGIN_SRC bash
❯ kubectl create deployment httpenv --image=bretfisher/httpenv
deployment.apps/httpenv created

❯ kubectl scale deployment/httpenv --replicas=5
deployment.apps/httpenv scaled

❯ kubectl expose deployment/httpenv --port 8888
service/httpenv exposed

❯ kubectl get services
NAME           TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)    AGE
httpenv        ClusterIP   10.0.119.179   <none>        8888/TCP   25h
kubernetes     ClusterIP   10.0.0.1       <none>        443/TCP    104d
oauth2-proxy   ClusterIP   10.0.29.165    <none>        4180/TCP   99d
#+END_SRC

Во 2 инстансе терминала:
#+BEGIN_SRC bash
❯ kubectl run tmp-shell --rm -it --image bretfisher/netshoot -- bash
If you don't see a command prompt, try pressing enter.
bash-5.0#
#+END_SRC

Что тут происходит? Ок, объясняю
=--rm= - удалить под после завершеия
=-it= - перенаправление tty в терминал
=--image= - собственное образ
=--= - означает что опции закончались, после слешей идет запускаемая команда в контейнере

Внутри 2 инстанса:
#+BEGIN_SRC bash
bash-5.0# curl httpenv:8888
{"HOME":"/root","HOSTNAME":"httpenv-6fdc8554fb-8kvcg","KUBERNETES_PORT":"tcp://10.0.0.1:443","KUBERNETES_PORT_443_TCP":"tcp://10.0.0.1:443","KUBERNETES_PORT_443_TCP_ADDR":"10.0.0.1","KUBERNETES_PORT_443_TCP_PORT":"443","KUBERNETES_PORT_443_TCP_PROTO":"tcp","KUBERNETES_SERVICE_HOST":"10.0.0.1","KUBERNETES_SERVICE_PORT":"443","KUBERNETES_SERVICE_PORT_HTTPS":"443","OAUTH2_PROXY_PORT":"tcp://10.0.29.165:4180","OAUTH2_PROXY_PORT_4180_TCP":"tcp://10.0.29.165:4180","OAUTH2_PROXY_PORT_4180_TCP_ADDR":"10.0.29.165","OAUTH2_PROXY_PORT_4180_TCP_PORT":"4180","OAUTH2_PROXY_PORT_4180_TCP_PROTO":"tcp","OAUTH2_PROXY_SERVICE_HOST":"10.0.29.165","OAUTH2_PROXY_SERVICE_PORT":"4180","OAUTH2_PROXY_SERVICE_PORT_HTTP":"4180","PATH":"/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"}
#+END_SRC
** CoreDNS
:PROPERTIES:
:ID: kubernetes-namespace
:END:
CoreDNS посталяется из коробки начиная с 1.11 версии. Предоставляет возможность для взаимодействия между сервисами через namespace, при этом namespace не должен пересекаться с именем пода.
/Когда речь идет про DNS то это исключительно алиасы на ip адреса *внутри кластера*/

=kubectl get namespace= - получить список всех неймспейсов.

#+START_SPOILER Пример дефолтных namespace >
#+BEGIN_SRC bash
❯ kubectl get namespaces
NAME              STATUS   AGE
artifactory       Active   105d
cert-manager      Active   105d
default           Active   105d
dev               Active   105d
igress            Active   105d
ingress           Active   105d
jenkins           Active   105d
kube-node-lease   Active   105d
kube-public       Active   105d
kube-system       Active   105d
oauth             Active   100d
prometheus        Active   105d
registry          Active   105d
staging           Active   82d
weave             Active   105d
#+END_SRC
#+CLOSE_SPOILER
Сервиса имеют свой [[https://ru.wikipedia.org/wiki/FQDN][FQDN]] вида: =curl <hostname>.<namespace>.svc.cluster.local=
где =svc= означает что это сервис, а =cluster.local= присваивается в момент инициализации кластера.
** Generators
:PROPERTIES:
:ID: kubernetes-generators
:END:
Команда для прверки того что будет создано: =--dry-run -o yaml=

#+BEGIN_SRC bash
kubectl create deployment test --image nginx --dry-run -o yaml
#+END_SRC

#+START_SPOILER вывод >
#+BEGIN_SRC bash
W1215 21:37:58.001810   94785 helpers.go:557] --dry-run is deprecated and can be replaced with --dry-run=client.
apiVersion: apps/v1
kind: Deployment
metadata:
  creationTimestamp: null
  labels:
    app: test
  name: test
spec:
  replicas: 1
  selector:
    matchLabels:
      app: test
  strategy: {}
  template:
    metadata:
      creationTimestamp: null
      labels:
        app: test
    spec:
      containers:
      - image: nginx
        name: nginx
        resources: {}
status: {}
#+END_SRC
#+CLOSE_SPOILER
Отличия от вывода JOB
#+BEGIN_SRC bash :results output
kubectl create job test --image nginx --dry-run -o yaml
#+END_SRC

#+RESULTS:
#+begin_example
apiVersion: batch/v1
kind: Job
metadata:
  creationTimestamp: null
  name: test
spec:
  template:
    metadata:
      creationTimestamp: null
    spec:
      containers:
      - image: nginx
        name: test
        resources: {}
      restartPolicy: Never
status: {}
#+end_example

#+BEGIN_SRC bash :results output :async
kubectl expose deployment/httpenv --port 80 --dry-run -o yaml
#+END_SRC

#+RESULTS:
#+begin_example
apiVersion: v1
kind: Service
metadata:
  creationTimestamp: null
  labels:
    app: httpenv
  name: httpenv
spec:
  ports:
  - port: 80
    protocol: TCP
    targetPort: 80
  selector:
    app: httpenv
status:
  loadBalancer: {}
#+end_example

*** Создание деплоймента и сразу же expose (как сервис)

#+BEGIN_SRC bash :results output :async
kubectl run test --image nginx --port 80 --expose --dry-run
#+END_SRC

#+START_SPOILER Пример >
#+RESULTS:
: service/test created (dry run)
: pod/test created (dry run)
#+CLOSE_SPOILER
*** Перезагрузка после падения
#+BEGIN_SRC bash :results output :async
kubectl run test --image nginx --restart OnFailure --dry-run -o yaml
#+END_SRC
*** TODO [#E] Никогда не перезагружать (вроде это дефолтное поведение)
#+BEGIN_SRC bash :results output
kubectl run test --image nginx --restart Never --dry-run -o yaml
#+END_SRC

#+RESULTS:
#+begin_example
apiVersion: v1
kind: Pod
metadata:
  creationTimestamp: null
  labels:
    run: test
  name: test
spec:
  containers:
  - image: nginx
    name: test
    resources: {}
  dnsPolicy: ClusterFirst
  restartPolicy: Never
status: {}
#+end_example

*** Schedule! Использование крон задач в кубере
Не пашет V
#+BEGIN_SRC bash :results output
kubectl create cronjob test --image nginx --schedule "*/1 * * * *" --dry-run
#+END_SRC

#+RESULTS:
: cronjob.batch/test created (dry run)
** Kubernetes storage
:PROPERTIES:
:ID: kubernetes-storage
:END:

+ =StatefulSets= - относительно новый тип, делающий поды более
+ Volumes - очень похожи на те, что есть в докер контейнере. Однако, в отличии от докер, они привязаны к времени жизни пода.
+ PersistentVolumets - создаются на уровне кластера, может быть разделен между несколькими подами
+ CSI plugin (container storage interface) - новый путь для связи между хранилищами
** TODO Ingress
:PROPERTIES:
:ID: ingress
:END:

Позволяет конфигурировать работу кластера на 7 уровне OSI - HTTP

:PROPERTIES:
:ID: kubernetes-ingress
:END:

:PROPERTIES:
:ID: kubernetes-ingress
:END:
*** Популярные проекси сервера
- Traefik
- HAProxy
- F5
- Envoy
- Istio
*** Установка
#+BEGIN_SRC bash
helm repo add ingress-nginx https://kubernetes.github.io/ingress-nginx
helm repo update
helm install ingress-nginx ingress-nginx/ingress-nginx
#+END_SRC

Однако, для bare metal, такой тип установки мне не подошел (не было соединения с host машины и снаружи кластера). Зато подошел такой:
#+BEGIN_SRC bash
helm install ingress-nginx ingress-nginx/ingress-nginx --set controller.hostNetwork=true --set controller.service.type=LoadBalancer
#+END_SRC
*** Установка Metallb (если нет внешнего балансировщика)
=kubectl edit configmap -n kube-system kube-proxy=
#+BEGIN_SRC bash
apiVersion: kubeproxy.config.k8s.io/v1alpha1
kind: KubeProxyConfiguration
mode: "ipvs"
ipvs:
    strictARP: true
#+END_SRC

После сохранения =helm install metallb metallb/metallb -f values.yaml=

*** Bare metall install problems
+ [[https://www.reddit.com/r/kubernetes/comments/a0wpip/kubernetes_ingress_with_metallb_not_working_what/][Похожая проблема с установкой на bare metal с metallb]]

** TODO CRD Operator Pattern
** TODO Kubernetes dashboard

#+BEGIN_SRC bash
NAME: ingress-nginx
LAST DEPLOYED: Mon Dec 20 18:04:32 2021
NAMESPACE: default
STATUS: deployed
REVISION: 1
TEST SUITE: None
NOTES:
The ingress-nginx controller has been installed.
It may take a few minutes for the LoadBalancer IP to be available.
You can watch the status by running 'kubectl --namespace default get services -o wide -w ingress-nginx-controller'

An example Ingress that makes use of the controller:
  apiVersion: networking.k8s.io/v1
  kind: Ingress
  metadata:
    name: example
    namespace: foo
  spec:
    ingressClassName: nginx
    rules:
      - host: www.example.com
        http:
          paths:
            - backend:
                service:
                  name: exampleService
                  port:
                    number: 80
              path: /
    # This section is only required if TLS is to be enabled for the Ingress
    tls:
      - hosts:
        - www.example.com
        secretName: example-tls

If TLS is enabled for the Ingress, a Secret containing the certificate and key must also be provided:

  apiVersion: v1
  kind: Secret
  metadata:
    name: example-tls
    namespace: foo
  data:
    tls.crt: <base64 encoded cert>
    tls.key: <base64 encoded key>
  type: kubernetes.io/tls
#+END_SRC

* Декларативное управление конфигурацией
:PROPERTIES:
:ID: kubernetes-declarative
:END:
Основная команда для применения имзенений =kubectl apply -f filename.yml=.
Команду можно применять к целой директории =kubectl apply -f mydir/=.
Либо url, =kubectl apply -f https://bret.run/pod.yaml= (имхо - абсолютно бесолезный кейс)

** Описание полей yaml файла
:PROPERTIES:
:ID: kubernetes-yaml
:END:
+ *kind* - тип текущего рекурса. Посмотреть все ресурсы можно с помощью =kubectl api-resources=
+ *apiVersions* - версия куба, можно посмотреть =kubectl api-versions=
+ *metaData* - поле для указания метаинформации, имя - обязательно
+ *spec* - хранение наших экшенов
*** Пример просторого yaml файла с конфигами :noexport:
#+BEGIN_SRC yaml
apiVersion: v1
kind: Pod
metadata:
  name: nginx
spec:
  containers:
    - name: nginx
      image: nginx:1.17.3
      ports:
        - containerPort: 80
#+END_SRC

В 1 файле можно описать несколько объектов через =---=
#+BEGIN_SRC yaml

apiVersion: v1
kind: Pod
metadata:
  name: nginx
spec:
  containers:
    - name: nginx
      image: nginx:1.17.3
      ports:
        - containerPort: 80
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: app-nginx-deployment
spec:
  replicas: 3
  selector:
    matchLabels:
      app: app-nginx
      dude: "true"
    spec:
      containers:
        - name: nginx
          image: nginx:1.17.3
  # ....
#+END_SRC
*** Подробнее про спецефикацию
В целом, вся спецификация доступна [[https://kubernetes.io/docs/reference/#api-reference][тут]], если по какой-то причине, вы так-же как и я, ненавидите браузеры, тогда можно использовать CLI
Посмотреть все поля спецификации можно с помощью =kubectl explain services --recursive=
Посмотреть только спецификацию с описанием =kubectl explain services.spec=
Посомтреть конкретное поле =kubectl explain services.spec.type=
Такие цепочки можно строить по всей схеме, например:

#+BEGIN_SRC bash
❯ kubectl explain deployment.spec.template.spec.volumes.nfs.server
KIND:     Deployment
VERSION:  apps/v1

FIELD:    server <string>

DESCRIPTION:
     Server is the hostname or IP address of the NFS server. More info:
     https://kubernetes.io/docs/concepts/storage/volumes#nfs
#+END_SRC
** Просмотр изменений, применяемых командой apply
*** Поверхностно отценить изменения
Для просмотра изменений можно использовать флаг =--dry-run=, он принимает аргументы:
- =--dry-run=client= - просмотр изменений со стороны клиента, без учета того, что уже находится на сервере (кажется весьма бесполезным)
- =--dry-run=server= - просмотр изменений со стороны сервера

#+START_SPOILER Пример конфиг файла app.yml >
#+BEGIN_SRC yaml
apiVersion: v1
kind: Service
metadata:
  name: app-nginx-service
spec:
  type: NodePort
  ports:
  - port: 80
  selector:
    app: app-nginx
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: app-nginx-deployment
spec:
  replicas: 3
  selector:
    matchLabels:
      app: app-nginx
  template:
    metadata:
      labels:
        app: app-nginx
    spec:
      containers:
      - name: nginx
        image: nginx:1.17.3
        ports:
        - containerPort: 80
#+END_SRC
#+CLOSE_SPOILER

Находясь в директории c данным файлом запустим его: =kubectl apply -f app.yml=
Теперь проверим изменения со стороны клиента
#+BEGIN_SRC bash
❯ kubectl apply -f app.yml --dry-run=client
kservice/app-nginx-service configured (dry run)
deployment.apps/app-nginx-deployment configured (dry run)

# И со стороны сервера
❯ kubectl apply -f app.yml --dry-run=server
service/app-nginx-service unchanged (server dry run)
deployment.apps/app-nginx-deployment unchanged (server dry run)
#+END_SRC

Однако это неочень то и информативно. Чтобы увидеть реальные изменения необходимо воспользоваться kubectl diff
*** Просмотр разницы между конфиг файлами
/Изменим app.yaml, изменив количество реплик и добавив какой-нибудь label/
=kubectl diff -f app.yml= Вывод команды весьма сильно похож на вывод диффа между комитами в git.
** Labels & annotations
*Labels* - список пар ключ/значение для идентификации вашего ресурсы позже, в select/group/filter операциях.
располагаются они в блоке metadata. Могут быть назначены практически на любой ресурс.
#+BEGIN_SRC yaml
# ...
metadata:
  name: app-nginx-deployment
  labels:
    tier: ftontend
    app: api
    env: prod
    customer: apple.com
# ...
#+END_SRC

Как применять? Например можно получить все nginx поды:
=kubectl get pods -l app=nginx=
=kubectl apply -f myfile.yaml -l app=nginx=

Также лейблы используются для нахождения необходимых подов, для этого используется ключ =selector= в спецификации.


* Slurm. Kubernetes.
:PROPERTIES:
:ID: slurm-kubernetes
:END:
** Основные ресурсы :slurm:
*** Replica set
  + Является шаблоном для подов
  + Исходя из вышесказанного - не имеет имени ресурса в шаблоне (т.к. назначает их сам, в процессе создания)
  + Следит за количеством реплик, при чем как в большую так и в меньшую сторону (удаляет лишние, если создались через pod)
  + Не следит за изменением мета информации (например изменение образа), только за количеством запущенных реплик!
*** Deployment
+ Решает проблему обновления приложения (по мета информации)
+ Под копотом создает ReplicaSet

**** Стратегии обновления
spec -> strategy
  =RollingUpdate= - стратегия по умолчанию, постепенное обновление реплик (в момент обновления что-то продолжает работать). Приложения должны быть обратно совместимы.
  =Reacreate= - полное удаление всех реплик и создание новых. Приводит к даунтайму.

  Для =RollingUpdate= можно применить дополнительные настройки:

  =maxSurge= - количество подов, на которое можно поднять единовременное текущий деплой
  =maxUnavailable= - противоположное значение, максимальное количество реплик на которое можно опустить текущий деплой.

  Можно установить в процентах

  #+BEGIN_SRC yaml
apiversion: apps/v1
kind: deployment
metadata:
  name: my-deployment
spec:
  replicas: 2
  strategy:
    rollingupdate:
      maxsurge: 10%
      maxunavailable: 10%
  #+END_SRC
*** Resources
Подробнее про ресурсы [[https://kubernetes.io/docs/tasks/configure-pod-container/assign-cpu-resource/][тут]]
1. Память
2. CPU
Бывают limits\requests

limits - сколько ресурсов максимально может быть использовано

out of memory killer (OOM) - если приложение попытается запросить больше памяти, кубер его убьет.

requests - резервируемые ресурсы на ноде. На основание ресурсов кубернетис определяет на какую ноду поместить под.

У Кластера есть capacity, по которому определяется размещение подов по requests.

# TODO: разобраться
/CPU указывается в единицах =m=/, mili cpu 1. Подробнее про них читать [[https://askinglot.com/what-is-millicpu][тут]]


**** QoS класс
- burstable - лимиты больше чем реквесты. Поды такого класса будут удоляться во 2 очередь (перемещаться на другую ноду)
- Garantued - Лимиты и реквесты равны. Кубер будет держать такие ноды до последнего.
- Best effort - никаких лимитов. Однако, в случае проблем с нодой (например нехватка ресурсов), такие ресурсы будут перемещены в 1 очередь.



Пример патча, хуй знает к чему оно тут, надо перенести.
#+BEGIN_SRC yaml
kubectl patch deployment my-deployment --patch '{"spec":{"template":{"spec":{"containers":[{"name":"nginx","resources":{"requests":{"cpu":"10"},"limits":{"cpu":"10"}}}]}}}}'
#+END_SRC
*** ConfigMap
Сущность для хранения настроек в виде пар ключ/значение. Позволяет использовать настройки в различных приложения.

Пример

#+BEGIN_SRC yaml
---
apiVersion: v1
kind: ConfigMap
metadata:
  name: my-configmap-env
data:
  dbhost: postgresql
  DEBUG: "false"
...

#+END_SRC

Посмотреть список config map можно командой =kubectl get configmap=

*Как использовать?*
Для использования config map достаточно в спецификации ресурса указать ключ =envFrom=


#+BEGIN_SRC yaml
# ...
        envFrom:
        - configMapRef:
            name: my-configmap-env
#+END_SRC

**** Получение информации из файла
#+BEGIN_SRC yaml
---
apiVersion: v1
kind: ConfigMap
metadata:
  name: my-configmap
data:
  default.conf: |
    server {
        listen       80 default_server;
        server_name  _;

        default_type text/plain;

        location / {
            return 200 '$hostname\n';
        }
    }
...
#+END_SRC

Использование текущего COnfigMap как volume:
#+START_SPOILER sample >
#+BEGIN_SRC yaml
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-deployment
spec:
  replicas: 1
  selector:
    matchLabels:
      app: my-app
  strategy:
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 1
    type: RollingUpdate
  template:
    metadata:
      labels:
        app: my-app
    spec:
      containers:
      - image: quay.io/testing-farm/nginx:1.12
        name: nginx
        ports:
        - containerPort: 80
        resources:
          requests:
            cpu: 10m
            memory: 100Mi
          limits:
            cpu: 100m
            memory: 100Mi
        volumeMounts:
        - name: config
          mountPath: /etc/nginx/conf.d/
      volumes:
      - name: config
        configMap:
          name: my-configmap
...
#+END_SRC
#+CLOSE_SPOILER

*** Secret
Сущность для хранения информации которую нельзя оглашать

+ generic - пароли/токены для приложений
+ docker-registry - данные авторизации в docker registry
+ tls - TLS сертификаты для ingress

Создание своего секрета
#+BEGIN_SRC bash
kubectl create secret generic test --from-literal=test1=asdf --from-literal=dbpassword=1q2w3e
 #+END_SRC

Информация хранится не в заширофваном виде, а в base64

Как получить значение секрета в переменной окружения?

#+BEGIN_SRC yaml
        env:
        - name: TEST
          value: foo
        - name: TEST_1
          valueFrom:
            secretKeyRef:
              name: test
              key: test1

#+END_SRC

**** String data
kubernetes получает все данные из этого раздела, затем под копотом формирует из них base-64

#+BEGIN_SRC yaml
---
apiVersion: v1
kind: Secret
metadata:
  name: test
stringData:
  test: updated
  anotherVar: hello world
...
#+END_SRC
*** DaemonSet
*** Downward API
[[https://kubernetes.io/docs/tasks/inject-data-application/downward-api-volume-expose-pod-information/][Прокидывание информации о поде в контейнер.]]

#+BEGIN_SRC yaml
        env:
        - name: TEST
          value: foo
        - name: TEST_1
          valueFrom:
            secretKeyRef:
              name: test
              key: test1
        - name: __NODE_NAME
          valueFrom:
            fieldRef:
              fieldPath: spec.nodeName
        - name: __POD_NAME
          valueFrom:
            fieldRef:
              fieldPath: metadata.name
#+END_SRC
*** Static POD
+ Единственный ресурс который ходит не в API напрямую
+ Запускаются перед всем остальным
+ Манифесты лежат на каждом узле
+ Необходимы для того чтобы кублет мог запустить что-то до того как поднялся апи сервер
*** Pod Anti Affinity :noexport:
+ Количество ресурсов жестко указано в манифесте деплоймента (при появлении новых узлов нужно менять значение, ту проблему решает демонсет)
+ Позволяет точно указать что каждый из подов запускается на 1 узле

  /Что-то вроде exclude/

**** Примеры
#+BEGIN_SRC yaml
affinity:
  podAntiAffinity:
    preferredDuringSchedulingIgnoredDuringExecution:
      - weight: 100
        podAffinityTerm:
          labelSelector:
            matchExpressions:
              - key: app
                operator: In
                values:
                  - rabbitmq
          topologyKey: kubernetes.io/hostname
#+END_SRC

*** Pod Affinity
+ Более гибкий вариант node selector
+ Бывают 2 типов: =affinity= и =podAntiAffinity=
+ /Усатый чел сказал что лучши этим не пользоваться, т.к. это не по кубернетес феншую/


**** Примеры
#+BEGIN_SRC yaml
affinity:
  podAntiAffinity:
    preferredDuringSchedulingIgnoredDuringExecution:
      nodeSelectorTerms:
      - matchExpression:
          - key: kubernetes.io/e2e-az-name
            operator: In
            values:
              - e2e-az1 # какая-то метка, которая ставится на узлы ^__^
              - e2e-az2 # под будет запущен либо на 1 либо на 2 узле с нужным матчингом

 #+END_SRC
 =preferredDuringSchedulingIgnoredDuringExecution= - обязательно выоплнить на этапе выполнения, но можно игнорировать при запуске

 #+BEGIN_SRC yaml
affinity:
  podAntiAffinity:
    preferredDuringSchedulingIgnoredDuringExecution:
      - weight: 1
        preference:
          matchExpressions:
            - key: another-node-label-key
              operator: Exists
 #+END_SRC
=preferredDuringSchedulingIgnoredDuringExecution= - постараться выполнить условие, если условие не удовлетворено, запустится "где-то"

*** Daemon Set
+ Количество реплик демонсета == количеству узлов в кластере
+ Запускает поды на всех нодах кластера
+ При добавлении ноды - добавляет поды
+ При удалении ноды GC удаляет под
+ Описание практчиески полностью соответствует *Deployment* (отсутствует поле количества реплик)

**** Пример
#+BEGIN_SRC yaml
---
apiVersion: apps/v1
kind: DaemonSet
metadata:
  labels:
    app: node-exporter
  name: node-exporter
spec:
  updateStrategy:
    rollingUpdate:
      maxUnavailable: 1
    type: RollingUpdate
  selector:
    matchLabels:
      app: node-exporter
  template:
    metadata:
      labels:
        app: node-exporter
    spec:
      containers:
      - name: node-exporter
        image: k8s.gcr.io/pause:3.3
        imagePullPolicy: IfNotPresent
        resources:
          limits:
            cpu: 10m
            memory: 64Mi
          requests:
            cpu: 10m
            memory: 64Mi
      nodeSelector:
        kubernetes.io/os: linux
      securityContext:
        runAsNonRoot: true
        runAsUser: 65534
      tolerations:
      - effect: NoSchedule
        key: node-role.kubernetes.io/ingress
#+END_SRC

Из интересного:
=taints= - кубернетис перестает запускать поды на данных узлах ;)
=tolerations= - но! кроме ресурсов описанных в tolerations
Ключ и занчения могут быть любыми, для построения сложных групп доступа/сопротивления

Эффекты: более специфическая вещь, влияющая на то, на каком этапе учитывается данные правила.
=NoSchedule= - учитывается *при размещении* на новом узле
=NoExecute= - действует еще и *на запущенные* ноды. Тоесть после навешивания на него taints, существующее поды, попадающие под правила, будут убиты (жестоко убиты! почти как декстером)
*** StatefulSet
+ Специальная абстракция для приложений, которые хранят свое состояние (бд/брокеры...)
+ Каждый из подов уникальный, и хранит свое состояние, имеет свое место.
+ Каждый под получает к имени индекс (и даже нумерация с 0! Совсем по взрослому)
+ Имеет =PersistentVolumeСlaimTemplate=
+ При удалении (даунгрейде) подов, =PV= остается.
*** Headless Service
+ Почти тоже самое что и clusterIP, однао имеет =.spec.clusterIP: non=
+ Нет правил трансляций в iptables
+ Имеет записи с именами ендпоинтов (DNS)

**** Пример
#+BEGIN_SRC yaml
---
kind: Service
apiVersion: v1
metadata:
  name: rabbitmq
  labels:
    app: rabbitmq
spec:
  clusterIP: None
  ports:
    - name: amqp
      protocol: TCP
      port: 5672
      targetPort: 5672
  selector:
    app: rabbitmq
#+END_SRC

** Хранение данных
*** HostPath
+ Самый простой, похож на то что имеется в докере. Монтирует каталог с хостовой фс к контейнеру.
+ Небезопасны. Т.к. могут получить доступ к системным каталогам на хост машине.

  #+BEGIN_SRC yaml
    spec:
      containers:
      - image: quay.io/testing-farm/nginx:1.12
        # ...
        volumeMounts:
        - name: data
          mountPath: /files
      volumes:
      - name: data
        hostPath:
          path: /data_pod
...
#+END_SRC
*** EmptyDir
+ Создает временный диск, который прокидывается внутрь контейнера
+ После удаления пода данные удаляются (но не после рестарта)
+ Создается для каждого пода в отдельности

  #+BEGIN_SRC yaml
    spec:
      containers:
      - image: quay.io/testing-farm/nginx:1.12
        name: nginx
        #...
        volumeMounts:
        - name: data
          mountPath: /files
      volumes:
      - name: data
        emptyDir: {}
  #+END_SRC
*** PV / PVC (Persistent volume)
+ Storage calss - хранит параметры подключения. Опциональное поле.
+ PersistentVolumeClaim - описывает требования к тому
+ PersistentVolume - хранит параметры и статус тома
+ Занимает диск целиком (даже если запросили меньше)
+ Reclaim policy: retain (данные сохраняются) и delete (данные удаляются)
  #+BEGIN_SRC yaml
volumes:
  - name: mypd
    persistentVolumaClaim:
      claimName: myclaim
  #+END_SRC

**** PV Provisioners
Утилита для автоматического создания дисков. При чем объем выделяется без излишек.

**** Sample
#+BEGIN_SRC yaml
---
kind: PersistentVolumeClaim
apiVersion: v1
metadata:
  name: fileshare
spec:
  storageClassName: csi-ceph-hdd-ms1
  accessModes:
  - ReadWriteMany
  resources:
    requests:
      storage: 10Mi
#+END_SRC

#+BEGIN_SRC text
❯ k apply -f pvc.yaml
persistentvolumeclaim/fileshare created
❯ k get pvc
NAME        STATUS   VOLUME                                     CAPACITY   ACCESS MODES   STORAGECLASS   AGE
fileshare   Bound    pvc-adf3594c-5e8f-4e9b-84b2-14d2aa499c7c   10Mi       RWX            hostpath       3s
#+END_SRC

Использование в deployment:
#+BEGIN_SRC yaml
#...
        volumeMounts:
        - name: config
          mountPath: /etc/nginx/conf.d
        - name: data
          mountPath: /data
      volumes:
      - name: config
        configMap:
          name: fileshare
      - name: data
        persistentVolumeClaim:
          claimName: fileshare
#+END_SRC
**** Увеличение размерности диска
** InitContainer
+ Подволяет выполнять настройкеи перед запуском основного контейнера
+ Выполняет по порядку описанному в манифесте
+ Можно монтировать те же тома, что и в основных контейнерах
+ Можно запускать от другого пользователя
** Аннотации :noexport::WIP:
Что-то вроде динамической конфигурации
** Сетевые абстракции
*** Probes
**** Liveness Probe
+ Контроль за состоянием приложения во время его жизни
+ Исполняется постоянно
**** Readiness Probe
+ Проверяет, готово ли приложение принимать трафик
+ В случае неудачного выполненися приложение убирается из балансировки
+ Исполняется постоянно
**** Startup Probe
+ Проверяет, запустилось ли приложение
+ Исполняет при старте
+ Выполняется перед другими пробами

Успешными считаются ответы от проба от *200* до *399*

*Методы проверок*: =httpGet=, =exec=, =tscSocket=

  #+BEGIN_SRC yaml
---
# file: practice/1.kube-basics-lecture/4.resources-and-probes/deployment-with-stuff.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: my-deployment
spec:
  replicas: 2
  selector:
    matchLabels:
      app: my-app
  strategy:
    rollingUpdate:
      maxSurge: 1
      maxUnavailable: 1
    type: RollingUpdate
  template:
    metadata:
      labels:
        app: my-app
    spec:
      containers:
      - image: quay.io/testing-farm/nginx:1.12
        name: nginx
        ports:
        - containerPort: 80
        readinessProbe:
          failureThreshold: 3
          httpGet:
            path: /
            port: 80
          periodSeconds: 10
          successThreshold: 1
          timeoutSeconds: 1
        livenessProbe:
          failureThreshold: 3
          httpGet:
            path: /
            port: 80
          periodSeconds: 10
          successThreshold: 1
          timeoutSeconds: 1
          initialDelaySeconds: 10
        startupProbe:
          httpGet:
            path: /
            port: 80
          failureThreshold: 30
          periodSeconds: 10
        resources:
          requests:
            cpu: 10m
            memory: 100Mi
          limits:
            cpu: 100m
            memory: 100Mi
...
  #+END_SRC
*** Сервисы
+ имеет Статисческий IP
+ создает DNS (myservice.mynamespace.svc.cluster.local)
+ По сути правила ipatables (или ipvc)
+ Service - не прокси!

**** ClusterIP (по умолчанию)
Внутрекластерное взаимодействие.
**** NodePort
Проброс внешних портов, от 30000 до 32768
**** LoadBalancer
Используется преимущественно у облочных провайдеров. Либо Metallb
**** ExternalName
Что-то вроде маршрутизации.
**** ExternalIPs
**** Headless service
Не указывается clusterIP, однако присваивается DNS. Используется для StatefullSet
*** Ingress (+ controller)
+ Proxy с nginx под копотом.
+ Напрямую перенаправляет запросы в под
+ В рамках ingress можно создавать множество хостов и путей


**** cert-manager :noexport:

** Устройство кластера
*** Master компоненты
**** Etcd
+ key-value база данных, работает на протоколе raf
**** API server
+ Центральный компонент Kubernetes
+ Работает через REST API (rest)
+ Авторизация и аутентификация происходит через API servier
**** Controller-manager
+ Состоит из бинарника ^_^
+ А также различны контроллеров:
  - Node controller (отслеживает доступность узлов кластера)
  - Replicaset controller
  - Endpoint controller (автоматическое создание ендпоинтов для сервисов)
  ...
+ Встроен garbage collector
+ Запускается по 1 на каждый узел, однако мастером становится лишь 1, тот кто успел первым. Если в течении времени, мастер ничего о себе не обновляет, то его вытесняет другой мастер. Короче все прямо как в жизни, если ты пассивный прокрастинатор, то рано или поздно тебя кто-то вытеснит 😅
**** Scheduler
Назначает поды на нодах. Принимает решение о том где и какой под будет запущен. Учитывате множество параметров, которые влияют на то, где на каком узле будет запущен под. Также, при прочих равных, шедулер умеет определять приоритет развертывания с учето того был ли скачен требуемый образ или нет (прям вау эффект).
Учитывает:
- Qos
- Affinity / anti-affinity (можно явно указать, какие поды могут быть на 1 узле, а какие нет)
- requested resource (равномерное распределение ресурсов между разными узлами)
- Подписан на событие создания новых подов. Дописывая после создания адрес ноды, на которой будет развернут под.

*** Worker компоненты
**** Kubelet
+ Работает на каждом кластере (даже на мастер нодах)
+ Не работает в контейнерах
+ Отдает команды докер демону
+ Создаем поды
+ Создает также пробы (хелсчеки, см выше)
**** Kube-proxy
+ Смотрит kube-api
+ Стоит на всех серверах
+ Управляет сетевыми правилами но нодах
+ В некоторых случаях не предустановлен (т.к. часто используются не коробочные решения)
** One shot tasks
+ Бекап раз в n времени
+ Миграции

** JOB
+ Запускат под копотом "особенный" под, который запускается, выполняется, и в финале, имеет статус complete.
+ Перезапускает поды до успешного выполнения задачи,  либо истечения таймаутов:
+ За job отвечает job controller, который входит в состав controller manager
+ JOB нельзя переаплаить, только пересоздать
+ Присутствует интересная бага, лимиты не всегда ограничивают по точному числу (иногда +-1)

*** Поля
- activeDeadLineSeconds (временной лимит)
- backoffLimit (количественный лимит)
- RestartPolicy - политика рестарта контейнеров (не подов)
- completions - хотябы n под должны быть выполнены успешно
- parallelism - сколлько подов запускать параллельно
- ttlSecondsAutoFinished - параметр который указывает на время ж

*** Пример простой JOB для вывода hello world
#+BEGIN_SRC yaml
apiVersion: batch/v1
kind: Job
metadata:
  name: hello
spec:
  backoffLimit: 2
  activeDeadlineSeconds: 60
  template:
    spec:
      containers:
      - name: hello
        image: quay.io/prometheus/busybox
        args:
        - /bin/sh
        - -c
        - date; echo Hello from the Kubernetes cluster
      restartPolicy: Never

#+END_SRC


- RestartPolicy - политика рестарта контейнеров (не подов)
** Cron JOB
+ Джобу можно преостановить, запустив =suspend= в true
+ Создает джобы (см выше)
+ Кронджобы должны быть идемпотентными
+ Будет использовать таймзону и время с =controller manager=
*** Параметры
- =schedule= - типичное описание крон джобы, например "*/1 * * * *"
- =concurrencyPolicy= - можно ли запускать новую джобу, если старая еще не завершилась. принимает =Allow=, =Forbid=, =replace=
- =successfulJobsHistoryLimit= - количество джобов, сохраняемых после успешного выполнения (сохраняются последние 3 джобы)
- =successfulJobsHistoryLimit= - количество заваленых джоб
- =startingDeadlineSeconds= - временной кредит который выдается джобе в случае отклонения от расписания. Естественно он должен быть меньше чем период выполнения кронджобы
** Аутентификация и авторизация в Kubernetes
*** RBAC (Roll Binding Access Control)
+ Поддерживает внешние сервисы аутентификации (например gitlab 🤷‍♂️)
+ Kubernetes умеет аутентифицировать по сертификатам (TLS, обычно это корневой сертификат кластера, с =CommonName= и =isOrganisation=)
**** RoleBinding
+ Служит для привязки ролей к разным сущностям (subjects)
+ Зависит от =namespace=

#+BEGIN_SRC yaml
rolRef:
  apiGroup: rbac.autrhization.k8s.io
  kind: Role # or ClusterRole
  name: ingree-ngin x
subjects:
  - kind: ServiceAccount
    name: ingress-nginx
    namespace: ingress-nginx
#+END_SRC

***** Пример
#+BEGIN_SRC yaml
---
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: user
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: view
subjects:
  - kind: ServiceAccount
    name: user
    namespace: s00001
#+END_SRC

=kg configmap --as=system:serviceaccount:s00001:user=

**** ClusterRole
**** ClusterRoleBinding
**** ServiceAccount
+ Придуманы для аутентификации приложений, который будут обараться к API кубера
+ При создании сервис аккаунта автоматически генерируется токен (JWT), подписываемый корневым сертификатом кластера

**** Role

Описывает права доступа к различным объектам кубернетис. Подробные [[https://kubernetes.io/docs/reference/access-authn-authz/authorization/][verbs тут]]

#+BEGIN_SRC yaml
- apiGroups: [""]
  resources: ["pods", "pods/log"]
  verbs: ["get", "list"]
#+END_SRC

Built-in roles:
В большинстве случаев используются следующие роли:
#+BEGIN_SRC bash
❯ kg clusterrole | egrep "^(cluster-)?admin|^view|^edit"
admin                                                                  2022-02-02T19:14:36Z
cluster-admin                                                          2022-02-02T19:14:36Z
edit                                                                   2022-02-02T19:14:36Z
view                                                                   2022-02-02T19:14:37Z
#+END_SRC
*** Контексты
Связывают кластера, пользователя и неймспейс. По умолчанию неймспейс - =default=
Использование: =k config set-context slurm.io= (можно указать либо имя, либо --current, что изменит неймспейс по умолчанию)
Краткая справка: =k config=
Вывод структуры: =k config view=
*** ResourceQuota
Устанавливает количество доступных ресуров и объектов для *неймспейса* в кластере. (Requiest/Limit/Количество ресурсов)
/В отличии от ResourceQuota, LimitRange ставится для пода/
*** Pod Security Policy
Запрещает добавлять поды с определенными проблемами безопасности (например поды с hostpath, либо поды работающие от рута и т.д)
Однако в ближайшее время это депрекейтнут 🤷‍♂️
* Debug kubernetes в production
** Проблемы 🥸
+ Часто в контейнере нет нужных команд
+ Запрет на exec в поде (потому что через него можно смотреть секреты)
+ Readonly файловая система

** И что делать!?
+ kubectl describe
+ kubectl get events /хранятся 1 час/
+ kubectl logs <pod_name> --previous
+ Ключ =terminationMessagePolicy: FallbacktoLogsOnError=, в describe будет добавлено 80 строчек лога кода (или) 
** Профилировщики
+ Большинство работают по сети
+ Нужно использоватть отдельный порт, либо роутинг на Ингрессе.
  /В целом штука опасная, т.к. можно заддосить через профайлер../
*** Python
  - Prometheus (/он, впринципе, для всего подряд/)
  - Rookout
*** Go
  - Pprof
* Autoscale
** HPA (Horizontal Pod Autoscaler)
+ Скейлит deployment через API запросы, увеличивая количество подов.
+ При использовании *HPA* необходимо всегда устанавливать =requests=
  /Связано это с принцепом работы HPA, как только ресурсы достигают 50%, создаются новые поды/
+ Изменение количества подов на понижение происходит не сразу (т.к. нагрузка мб неравномерной), по дефолту время до снижения количества подов - *5 минут*, его можно контролировать в

  Autoscale через cli
  =kubectl autoscale deployment php-apache --cpu-percent=50 --min=1 --max=5=

  #+BEGIN_SRC bash
❯ k get hpa
NAME         REFERENCE               TARGETS         MINPODS   MAXPODS   REPLICAS   AGE
php-apache   Deployment/php-apache   <unknown>/50%   1         5         1          28s
  #+END_SRC

*** Metric server
+ На каждой ноде где установлен kubelet поднимается
+ Метрики собираются по дефолту кублетом, однако их не собирает центральный узел кубернетиса
+ Чтобы собирать данные метрики как раз и нужен *Metric server*
+ Не хранит данные, предоставляет их в realtime
  Схема сия чуда:
  =k get apiservices.apiregistration.k8s.io v1beta1.metrics.k8s.io -o yaml=
*** HPA v2
Объекты метрик:
+ Resource (AverageUtilization, AverageValue):
  - CPU
  - Memory
+ Pods
  Можно реализовать внешний api, который позволит по внутренним метрикам определить нужно ли скейлить приложение
+ Object
  - Позволяет распределять скейлинг через метрики объектов кубернетиса
    /Например мы можем следить за количество запросов в ingress, и при достижении определенного количества скейлить наши поды/
+ External - скейлинг на основе внешних факторов, которые существуют за рамками кластера.
  /Например можно брать ресурсы из api, либо какой-то очереди, и при ее заполнении создавать поды-воркеры для выполнения каких-то тяжеловесных вычислени й/
** Cluster Autoscaler
+ Работает только на облачных серверах
+ Автоматически добавляет новые узлы в кластер
+ Реализуется на стороне провайдера
+ Может быть использован не столько совместно с HPA
+ Используется для динамических раннеров CI/CD, стендов.
** Vertical POD autoscaler
+ Увеличивает количество лимитов и реквестов на подах.
+ Используется редко.
+ Умеет следить за метриками, проставлять аннотацию =recommendations=
`;
    const result = parse(orgDoc);

    expect(result.toString()).toMatchInlineSnapshot();
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });
});
