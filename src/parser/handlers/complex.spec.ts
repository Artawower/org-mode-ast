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
: Hello World?
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
      "root [0-31857]
        newLine [0-1]
        property [1-13]
          text [1-13] (":PROPERTIES:")
        newLine [13-14]
        property [14-24]
          text [14-18] (":ID:")
          text [18-24] (" elisp")
        newLine [24-25]
        property [25-41]
          text [25-36] (":PUBLISHED:")
          text [36-41] (" true")
        newLine [41-42]
        property [42-47]
          text [42-47] (":END:")
        newLine [47-48]
        keyword [48-63]
          text [48-56] ("#+TITLE:")
          text [56-63] (" Elisp.")
        newLine [63-64]
        keyword [64-126]
          text [64-78] ("#+DESCRIPTION:")
          text [78-126] (" (message \\"Язык состоящий на 30% из смайликов\\").")
        newLine [126-127]
        keyword [127-138]
          text [127-132] ("#+ID:")
          text [132-138] (" elisp")
        newLine [138-139]
        keyword [139-159]
          text [139-148] ("#+AUTHOR:")
          text [148-159] (" Darkawower")
        newLine [159-160]
        keyword [160-178]
          text [160-170] ("#+STARTUP:")
          text [170-178] (" content")
        newLine [178-179]
        keyword [179-210]
          text [179-190] ("#+FILETAGS:")
          text [190-191] (" ")
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
          text [211-220] ("#+ACTIVE:")
          text [220-225] (" Yep!")
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
        headline [308-28590]
            :level 1:
          title [308-361]
            operator [308-310] ("* ")
            text [310-360] ("Elisp - расширеяемый функциональный язык для emacs")
            newLine [360-361]
          section [361-28590]
            text [361-537] ("Все что я опишу ниже - плод моего изучения. Рекомендую изучать язык по ссылкам приведенным ниже. Я могу ошибаться, а также неправильно интерпритировать изученный мной материал.")
            newLine [537-538]
            text [538-784] ("Также, может показаться что я отношусь к лиспу как к неочень хорошо спроектированному языку. Это не так. Я отношусь так ко всем языкам. При этом автор понятия не имеет как можно что-то улучшить, и вообще... не стоит тратить время на его писульки.")
            newLine [784-785]
            newLine [785-786]
            headline [786-28590]
                :level 2:
              title [786-796]
                operator [786-789] ("** ")
                text [789-795] ("Ссылки")
                newLine [795-796]
              section [796-28590]
                keyword [796-839]
                  text [796-839] ("#+START_{SPOILER} Ресурсы для ознакомления ")
                text [839-840] (">")
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
                keyword [1989-2006]
                  text [1989-2006] ("#+CLOSE_{SPOILER}")
                newLine [2006-2007]
                headline [2007-28590]
                    :level 2:
                  title [2007-2041]
                    operator [2007-2010] ("** ")
                    text [2010-2040] ("Пакеты в помощь для разработки")
                    newLine [2040-2041]
                  section [2041-28590]
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
                    headline [2216-28590]
                        :level 1:
                      title [2216-2231]
                        operator [2216-2218] ("* ")
                        text [2218-2230] ("Quick Start.")
                        newLine [2230-2231]
                      section [2231-28590]
                        text [2231-2283] ("Быстрый старт для тех кто уже умеет программировать.")
                        newLine [2283-2284]
                        newLine [2284-2285]
                        headline [2285-10535]
                            :level 2:
                          title [2285-2323]
                            operator [2285-2288] ("** ")
                            text [2288-2322] ("Типы данных, переменные, константы")
                            newLine [2322-2323]
                          section [2323-10535]
                            keyword [2323-2354]
                              text [2323-2354] ("#+START_{SPOILER} Основа языка ")
                            text [2354-2355] (">")
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
                                  blockHeader [2678-2700]
                                    keyword [2678-2700]
                                      text [2678-2689] ("#+begin_src")
                                      text [2689-2700] (" emacs-lisp")
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
                                text [2819-2820] (" ")
                                list [2820-2859]
                                    :unordered:
                                    :level 0:
                                  listItem [2820-2859]
                                    title [2820-2859]
                                      operator [2820-2822] ("- ")
                                      text [2822-2858] ("специального меню с ui полями ввода.")
                                      newLine [2858-2859]
                                text [2859-2883] ("Значение для переменной ")
                                verbatim [2883-2894]
                                  operator [2883-2884] ("=")
                                  text [2884-2893] ("defcustom")
                                  operator [2893-2894] ("=")
                                text [2894-3020] (" можно выбирать из списка: =:options=. Разработчик плагина может заранее задать список возможных значений для таких перменных.")
                                newLine [3020-3021]
                                text [3021-3030] ("=:group= ")
                                list [3030-3138]
                                    :unordered:
                                    :level 0:
                                  listItem [3030-3138]
                                    title [3030-3138]
                                      operator [3030-3032] ("- ")
                                      text [3032-3137] ("значение которое позволяет группировать несколько переменных в группу, для более удобного редактирования.")
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
                                  blockHeader [3186-3208]
                                    keyword [3186-3208]
                                      text [3186-3197] ("#+begin_src")
                                      text [3197-3208] (" emacs-lisp")
                                  newLine [3208-3209]
                                  blockBody [3209-3340]
                                    text [3209-3340] ("(defcustom my-custom-variable \\"hello\\"\\n  \\"Some description\\"\\n  :type 'string\\n  :group 'my-custom-group)\\n\\n(message my-custom-variable)")
                                  newLine [3340-3341]
                                  blockFooter [3341-3350]
                                    keyword [3341-3350]
                                      text [3341-3350] ("#+end_src")
                                newLine [3350-3351]
                                keyword [3351-3362]
                                  text [3351-3361] ("#+RESULTS:")
                                  text [3361-3362] (" ")
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
                                  blockHeader [3538-3560]
                                    keyword [3538-3560]
                                      text [3538-3549] ("#+begin_src")
                                      text [3549-3560] (" emacs-lisp")
                                  newLine [3560-3561]
                                  blockBody [3561-3625]
                                    text [3561-3625] ("  (let ((my-var \\"I'am a local variable\\"))\\n     (message my-var))")
                                  newLine [3625-3626]
                                  blockFooter [3626-3635]
                                    keyword [3626-3635]
                                      text [3626-3635] ("#+end_src")
                                newLine [3635-3636]
                                keyword [3636-3647]
                                  text [3636-3646] ("#+RESULTS:")
                                  text [3646-3647] (" ")
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
                                  blockHeader [3844-3866]
                                    keyword [3844-3866]
                                      text [3844-3855] ("#+begin_src")
                                      text [3855-3866] (" emacs-lisp")
                                  newLine [3866-3867]
                                  blockBody [3867-3987]
                                    text [3867-3987] ("(let* ((my-var \\"I'am a local variable\\")\\n       (my-var (concat my-var \\" And i can be overwrited!\\")))\\n  (message my-var))")
                                  newLine [3987-3988]
                                  blockFooter [3988-3997]
                                    keyword [3988-3997]
                                      text [3988-3997] ("#+end_src")
                                newLine [3997-3998]
                                keyword [3998-4009]
                                  text [3998-4008] ("#+RESULTS:")
                                  text [4008-4009] (" ")
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
                                  blockHeader [4201-4223]
                                    keyword [4201-4223]
                                      text [4201-4212] ("#+begin_src")
                                      text [4212-4223] (" emacs-lisp")
                                  newLine [4223-4224]
                                  blockBody [4224-4302]
                                    text [4224-4302] ("(let* ((name \\"Oleg\\"))\\n  (message name)\\n  (setq name \\"Vasya\\")\\n  (message name))")
                                  newLine [4302-4303]
                                  blockFooter [4303-4312]
                                    keyword [4303-4312]
                                      text [4303-4312] ("#+end_src")
                                newLine [4312-4313]
                                keyword [4313-4324]
                                  text [4313-4323] ("#+RESULTS:")
                                  text [4323-4324] (" ")
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
                                  blockHeader [4413-4435]
                                    keyword [4413-4435]
                                      text [4413-4424] ("#+begin_src")
                                      text [4424-4435] (" emacs-lisp")
                                  newLine [4435-4436]
                                  blockBody [4436-4601]
                                    text [4436-4601] ("(when-let* ((b 4)\\n            (d nil))\\n  (message \\"This code never will be executed\\"))\\n\\n(when-let* ((b 4)\\n            (d \\"He\\"))\\n  (message \\"But this code will be!\\"))")
                                  newLine [4601-4602]
                                  blockFooter [4602-4611]
                                    keyword [4602-4611]
                                      text [4602-4611] ("#+end_src")
                                newLine [4611-4612]
                                keyword [4612-4623]
                                  text [4612-4622] ("#+RESULTS:")
                                  text [4622-4623] (" ")
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
                                  blockHeader [4828-4850]
                                    keyword [4828-4850]
                                      text [4828-4839] ("#+begin_src")
                                      text [4839-4850] (" emacs-lisp")
                                  newLine [4850-4851]
                                  blockBody [4851-4995]
                                    text [4851-4995] ("\\n(let ((my-awesome-char ?Q))\\n              (message (string my-awesome-char ?H ?e ?e ?l ?o))\\n              (message (concat '(?W ?o ?r ?l ?d))))")
                                  newLine [4995-4996]
                                  blockFooter [4996-5005]
                                    keyword [4996-5005]
                                      text [4996-5005] ("#+end_src")
                                newLine [5005-5006]
                                keyword [5006-5017]
                                  text [5006-5016] ("#+RESULTS:")
                                  text [5016-5017] (" ")
                                newLine [5017-5018]
                                fixedWidth [5018-5025]
                                  operator [5018-5020] (": ")
                                  text [5020-5025] ("World")
                                newLine [5025-5026]
                                text [5026-5065] ("С помощью символов можно сделать repeat")
                                newLine [5065-5066]
                                newLine [5066-5067]
                                srcBlock [5067-5119]
                                  blockHeader [5067-5089]
                                    keyword [5067-5089]
                                      text [5067-5078] ("#+begin_src")
                                      text [5078-5089] (" emacs-lisp")
                                  newLine [5089-5090]
                                  blockBody [5090-5109]
                                    text [5090-5109] ("(make-string 10 ?|)")
                                  newLine [5109-5110]
                                  blockFooter [5110-5119]
                                    keyword [5110-5119]
                                      text [5110-5119] ("#+end_src")
                                newLine [5119-5120]
                                keyword [5120-5131]
                                  text [5120-5130] ("#+RESULTS:")
                                  text [5130-5131] (" ")
                                newLine [5131-5132]
                                fixedWidth [5132-5144]
                                  operator [5132-5134] (": ")
                                  text [5134-5144] ("||||||||||")
                                newLine [5144-5145]
                            headline [5145-5293]
                                :level 3:
                              title [5145-5168]
                                operator [5145-5149] ("*** ")
                                text [5149-5167] ("Работа со строками")
                                newLine [5167-5168]
                              section [5168-5293]
                                bold [5168-5191]
                                  operator [5168-5169] ("*")
                                  text [5169-5190] ("Форматирование строки")
                                  operator [5190-5191] ("*")
                                newLine [5191-5192]
                                newLine [5192-5193]
                                srcBlock [5193-5265]
                                  blockHeader [5193-5215]
                                    keyword [5193-5215]
                                      text [5193-5204] ("#+begin_src")
                                      text [5204-5215] (" emacs-lisp")
                                  newLine [5215-5216]
                                  blockBody [5216-5255]
                                    text [5216-5255] ("(message (format \\"Hello %s\\n\\" \\"World?\\"))")
                                  newLine [5255-5256]
                                  blockFooter [5256-5265]
                                    keyword [5256-5265]
                                      text [5256-5265] ("#+end_src")
                                newLine [5265-5266]
                                keyword [5266-5277]
                                  text [5266-5276] ("#+RESULTS:")
                                  text [5276-5277] (" ")
                                newLine [5277-5278]
                                fixedWidth [5278-5292]
                                  operator [5278-5280] (": ")
                                  text [5280-5292] ("Hello World?")
                                newLine [5292-5293]
                            headline [5293-7758]
                                :level 3:
                              title [5293-5304]
                                operator [5293-5297] ("*** ")
                                text [5297-5303] ("Списки")
                                newLine [5303-5304]
                              section [5304-7758]
                                text [5304-5467] ("Списки \\"экранируются\\" (на самом деле это не экранирование, т.к. все в лиспе функция это просто указатель на то что это не нужно исполнять, называется это evaluate ")
                                italic [5467-5495]
                                  operator [5467-5468] ("/")
                                  text [5468-5494] ("но это конечно же не точно")
                                  operator [5494-5495] ("/")
                                text [5495-5515] (") с помощью симола '")
                                newLine [5515-5516]
                                newLine [5516-5517]
                                srcBlock [5517-5591]
                                  blockHeader [5517-5539]
                                    keyword [5517-5539]
                                      text [5517-5528] ("#+begin_src")
                                      text [5528-5539] (" emacs-lisp")
                                  newLine [5539-5540]
                                  blockBody [5540-5581]
                                    text [5540-5581] ("(setq my-first-list '(\\"Foo\\" \\"Baz\\" \\"Qwe\\"))")
                                  newLine [5581-5582]
                                  blockFooter [5582-5591]
                                    keyword [5582-5591]
                                      text [5582-5591] ("#+end_src")
                                newLine [5591-5592]
                                headline [5592-5692]
                                    :level 4:
                                  title [5592-5621]
                                    operator [5592-5597] ("**** ")
                                    text [5597-5620] ("Получить первый элемент")
                                    newLine [5620-5621]
                                  section [5621-5692]
                                    srcBlock [5621-5673]
                                      blockHeader [5621-5643]
                                        keyword [5621-5643]
                                          text [5621-5632] ("#+begin_src")
                                          text [5632-5643] (" emacs-lisp")
                                      newLine [5643-5644]
                                      blockBody [5644-5663]
                                        text [5644-5663] ("(car my-first-list)")
                                      newLine [5663-5664]
                                      blockFooter [5664-5673]
                                        keyword [5664-5673]
                                          text [5664-5673] ("#+end_src")
                                    newLine [5673-5674]
                                    keyword [5674-5685]
                                      text [5674-5684] ("#+RESULTS:")
                                      text [5684-5685] (" ")
                                    newLine [5685-5686]
                                    fixedWidth [5686-5691]
                                      operator [5686-5688] (": ")
                                      text [5688-5691] ("Foo")
                                    newLine [5691-5692]
                                headline [5692-5788]
                                    :level 4:
                                  title [5692-5735]
                                    operator [5692-5697] ("**** ")
                                    text [5697-5734] ("Получить все кроме первого элемента..")
                                    newLine [5734-5735]
                                  section [5735-5788]
                                    srcBlock [5735-5787]
                                      blockHeader [5735-5757]
                                        keyword [5735-5757]
                                          text [5735-5746] ("#+begin_src")
                                          text [5746-5757] (" emacs-lisp")
                                      newLine [5757-5758]
                                      blockBody [5758-5777]
                                        text [5758-5777] ("(cdr my-first-list)")
                                      newLine [5777-5778]
                                      blockFooter [5778-5787]
                                        keyword [5778-5787]
                                          text [5778-5787] ("#+end_src")
                                    newLine [5787-5788]
                                headline [5788-6280]
                                    :level 4:
                                  title [5788-5819]
                                    operator [5788-5793] ("**** ")
                                    text [5793-5818] ("Добавить элемент в список")
                                    newLine [5818-5819]
                                  section [5819-6280]
                                    text [5819-5909] ("Push мутирует список. Не выглядит как нечто функциональное, но возможно я что-то не понял.")
                                    newLine [5909-5910]
                                    newLine [5910-5911]
                                    srcBlock [5911-6027]
                                      blockHeader [5911-5933]
                                        keyword [5911-5933]
                                          text [5911-5922] ("#+begin_src")
                                          text [5922-5933] (" emacs-lisp")
                                      newLine [5933-5934]
                                      blockBody [5934-6017]
                                        text [5934-6017] ("(setq my-first-list '())\\n(push \\"Lalalend\\" my-first-list)\\n(push \\"Hey\\" my-first-list)")
                                      newLine [6017-6018]
                                      blockFooter [6018-6027]
                                        keyword [6018-6027]
                                          text [6018-6027] ("#+end_src")
                                    newLine [6027-6028]
                                    table [6028-6047]
                                      tableRow [6028-6046]
                                        operator [6028-6029] ("|")
                                        tableCell [6029-6034]
                                          text [6029-6034] (" Hey ")
                                        operator [6034-6035] ("|")
                                        tableCell [6035-6045]
                                          text [6035-6045] (" Lalalend ")
                                        operator [6045-6046] ("|")
                                      newLine [6046-6047]
                                    text [6047-6099] ("Ну или так (последний аргумент t - добавить в конец)")
                                    newLine [6099-6100]
                                    newLine [6100-6101]
                                    srcBlock [6101-6245]
                                      blockHeader [6101-6123]
                                        keyword [6101-6123]
                                          text [6101-6112] ("#+begin_src")
                                          text [6112-6123] (" emacs-lisp")
                                      newLine [6123-6124]
                                      blockBody [6124-6235]
                                        text [6124-6235] ("(setq my-test-2-list '(\\"qweqweqwe\\" \\"123\\"))\\n(add-to-list 'my-test-2-list \\"qwe\\" t)\\n\\n(message \\"%s\\" my-test-2-list)")
                                      newLine [6235-6236]
                                      blockFooter [6236-6245]
                                        keyword [6236-6245]
                                          text [6236-6245] ("#+end_src")
                                    newLine [6245-6246]
                                    keyword [6246-6257]
                                      text [6246-6256] ("#+RESULTS:")
                                      text [6256-6257] (" ")
                                    newLine [6257-6258]
                                    fixedWidth [6258-6279]
                                      operator [6258-6260] (": ")
                                      text [6260-6279] ("(qweqweqwe 123 qwe)")
                                    newLine [6279-6280]
                                headline [6280-6482]
                                    :level 4:
                                  title [6280-6303]
                                    operator [6280-6285] ("**** ")
                                    text [6285-6302] ("Слияние 2 списков")
                                    newLine [6302-6303]
                                  section [6303-6482]
                                    srcBlock [6303-6451]
                                      blockHeader [6303-6325]
                                        keyword [6303-6325]
                                          text [6303-6314] ("#+begin_src")
                                          text [6314-6325] (" emacs-lisp")
                                      newLine [6325-6326]
                                      blockBody [6326-6441]
                                        text [6326-6441] ("(setq my-first-list '(?q ?b ?c))\\n(setq my-first-list (append my-first-list (list ?t)))\\n(message \\"%s\\" my-first-list)")
                                      newLine [6441-6442]
                                      blockFooter [6442-6451]
                                        keyword [6442-6451]
                                          text [6442-6451] ("#+end_src")
                                    newLine [6451-6452]
                                    keyword [6452-6463]
                                      text [6452-6462] ("#+RESULTS:")
                                      text [6462-6463] (" ")
                                    newLine [6463-6464]
                                    fixedWidth [6464-6481]
                                      operator [6464-6466] (": ")
                                      text [6466-6481] ("(113 98 99 116)")
                                    newLine [6481-6482]
                                headline [6482-6693]
                                    :level 4:
                                  title [6482-6491]
                                    operator [6482-6487] ("**** ")
                                    text [6487-6490] ("Map")
                                    newLine [6490-6491]
                                  section [6491-6693]
                                    text [6491-6505] ("На самом деле ")
                                    verbatim [6505-6513]
                                      operator [6505-6506] ("=")
                                      text [6506-6512] ("mapcar")
                                      operator [6512-6513] ("=")
                                    text [6513-6514] (" ")
                                    crossed [6514-6565]
                                      operator [6514-6515] ("+")
                                      text [6515-6564] ("(возможно создатель языка хотел иметь машину...).")
                                      operator [6564-6565] ("+")
                                    newLine [6565-6566]
                                    newLine [6566-6567]
                                    srcBlock [6567-6692]
                                      blockHeader [6567-6589]
                                        keyword [6567-6589]
                                          text [6567-6578] ("#+begin_src")
                                          text [6578-6589] (" emacs-lisp")
                                      newLine [6589-6590]
                                      blockBody [6590-6682]
                                        text [6590-6682] ("  (defun greeting (name)\\n    (format \\"Hello %s\\" name))\\n  \\n  (mapcar 'greeting my-first-list)")
                                      newLine [6682-6683]
                                      blockFooter [6683-6692]
                                        keyword [6683-6692]
                                          text [6683-6692] ("#+end_src")
                                    newLine [6692-6693]
                                headline [6693-6953]
                                    :level 4:
                                  title [6693-6706]
                                    operator [6693-6698] ("**** ")
                                    text [6698-6705] ("forEach")
                                    newLine [6705-6706]
                                  section [6706-6953]
                                    verbatim [6706-6713]
                                      operator [6706-6707] ("=")
                                      text [6707-6712] ("mpcar")
                                      operator [6712-6713] ("=")
                                    text [6713-6790] (" создает новый список, можно просто итерироваться по записям с помощь. dolist")
                                    newLine [6790-6791]
                                    newLine [6791-6792]
                                    srcBlock [6792-6923]
                                      blockHeader [6792-6814]
                                        keyword [6792-6814]
                                          text [6792-6803] ("#+begin_src")
                                          text [6803-6814] (" emacs-lisp")
                                      newLine [6814-6815]
                                      blockBody [6815-6913]
                                        text [6815-6913] ("(let* ((v \\"\\"))\\n\\n  (dolist (p '(\\"one\\" \\"two\\" \\"three\\"))\\n    (setq v (concat v \\" \\" p)))\\n  (message v))")
                                      newLine [6913-6914]
                                      blockFooter [6914-6923]
                                        keyword [6914-6923]
                                          text [6914-6923] ("#+end_src")
                                    newLine [6923-6924]
                                    keyword [6924-6935]
                                      text [6924-6934] ("#+RESULTS:")
                                      text [6934-6935] (" ")
                                    newLine [6935-6936]
                                    fixedWidth [6936-6952]
                                      operator [6936-6938] (": ")
                                      text [6938-6952] (" one two three")
                                    newLine [6952-6953]
                                headline [6953-7069]
                                    :level 4:
                                  title [6953-6993]
                                    operator [6953-6958] ("**** ")
                                    text [6958-6992] ("Проверить есть ли элемент в списке")
                                    newLine [6992-6993]
                                  section [6993-7069]
                                    srcBlock [6993-7060]
                                      blockHeader [6993-7015]
                                        keyword [6993-7015]
                                          text [6993-7004] ("#+begin_src")
                                          text [7004-7015] (" emacs-lisp")
                                      newLine [7015-7016]
                                      blockBody [7016-7050]
                                        text [7016-7050] ("(member \\"123\\" '(1233 \\"qwe\\" \\"123\\"))")
                                      newLine [7050-7051]
                                      blockFooter [7051-7060]
                                        keyword [7051-7060]
                                          text [7051-7060] ("#+end_src")
                                    newLine [7060-7061]
                                    table [7061-7069]
                                      tableRow [7061-7068]
                                        operator [7061-7062] ("|")
                                        tableCell [7062-7067]
                                          text [7062-7067] (" 123 ")
                                        operator [7067-7068] ("|")
                                      newLine [7068-7069]
                                headline [7069-7503]
                                    :level 4:
                                  title [7069-7115]
                                    operator [7069-7074] ("**** ")
                                    text [7074-7114] ("Перезаписать элемент в списке по индексу")
                                    newLine [7114-7115]
                                  section [7115-7503]
                                    srcBlock [7115-7256]
                                      blockHeader [7115-7137]
                                        keyword [7115-7137]
                                          text [7115-7126] ("#+begin_src")
                                          text [7126-7137] (" emacs-lisp")
                                      newLine [7137-7138]
                                      blockBody [7138-7246]
                                        text [7138-7246] ("(setq my-test-list '((\\"qwe\\" . (setcdr (assoc \\"qwe\\" my-test-list) \\"asdlkajsdakd\\")\\n(message \\"%s\\" my-test-list)")
                                      newLine [7246-7247]
                                      blockFooter [7247-7256]
                                        keyword [7247-7256]
                                          text [7247-7256] ("#+end_src")
                                    newLine [7256-7257]
                                    keyword [7257-7268]
                                      text [7257-7267] ("#+RESULTS:")
                                      text [7267-7268] (" ")
                                    newLine [7268-7269]
                                    fixedWidth [7269-7302]
                                      operator [7269-7271] (": ")
                                      text [7271-7302] ("((qwe . asdlkajsdakd) (be . 2))")
                                    newLine [7302-7303]
                                    newLine [7303-7304]
                                    text [7304-7343] ("А что если этого элемента нет в списке?")
                                    newLine [7343-7344]
                                    srcBlock [7344-7489]
                                      blockHeader [7344-7366]
                                        keyword [7344-7366]
                                          text [7344-7355] ("#+begin_src")
                                          text [7355-7366] (" emacs-lisp")
                                      newLine [7366-7367]
                                      blockBody [7367-7479]
                                        text [7367-7479] ("(setq my-test-list '((\\"be\\" . 2)))\\n(setcdr (assoc \\"qwe\\" my-test-list) \\"asdlkajsdakd\\")\\n(message \\"%s\\" my-test-list)")
                                      newLine [7479-7480]
                                      blockFooter [7480-7489]
                                        keyword [7480-7489]
                                          text [7480-7489] ("#+end_src")
                                    newLine [7489-7490]
                                    newLine [7490-7491]
                                    text [7491-7502] ("Не работает")
                                    newLine [7502-7503]
                                headline [7503-7758]
                                    :level 4:
                                  title [7503-7534]
                                    operator [7503-7508] ("**** ")
                                    text [7508-7533] ("Удалить элемент из списка")
                                    newLine [7533-7534]
                                  section [7534-7758]
                                    srcBlock [7534-7755]
                                      blockHeader [7534-7572]
                                        keyword [7534-7557]
                                          text [7534-7545] ("#+BEGIN_SRC")
                                          text [7545-7557] (" emacs-lisp ")
                                        blockProperty [7557-7572]
                                          text [7557-7565] (":results")
                                          text [7565-7572] (" silent")
                                      newLine [7572-7573]
                                      blockBody [7573-7745]
                                        text [7573-7745] ("ELISP> (setq list1 '(alpha beta gamma))\\n (alpha beta gamma)\\n \\n ELISP> (setq list2 (delete 'beta list1))\\n (alpha gamma)\\n \\n ELISP> (setq list3 (delete 'alpha list1))\\n (gamma)")
                                      newLine [7745-7746]
                                      blockFooter [7746-7755]
                                        keyword [7746-7755]
                                          text [7746-7755] ("#+END_SRC")
                                    newLine [7755-7756]
                                    newLine [7756-7757]
                                    newLine [7757-7758]
                            headline [7758-9729]
                                :level 3:
                              title [7758-7784]
                                operator [7758-7762] ("*** ")
                                text [7762-7783] ("Ассоциативные массивы")
                                newLine [7783-7784]
                              section [7784-9729]
                                headline [7784-8095]
                                    :level 4:
                                  title [7784-7800]
                                    operator [7784-7789] ("**** ")
                                    text [7789-7799] ("Объявление")
                                    newLine [7799-7800]
                                  section [7800-8095]
                                    srcBlock [7800-7851]
                                      blockHeader [7800-7822]
                                        keyword [7800-7822]
                                          text [7800-7811] ("#+begin_src")
                                          text [7811-7822] (" emacs-lisp")
                                      newLine [7822-7823]
                                      blockBody [7823-7842]
                                        text [7823-7842] ("(setq trees '((a . ")
                                      blockFooter [7842-7851]
                                        keyword [7842-7851]
                                          text [7842-7851] ("#+end_src")
                                    newLine [7851-7852]
                                    keyword [7852-7863]
                                      text [7852-7862] ("#+RESULTS:")
                                      text [7862-7863] (" ")
                                    newLine [7863-7864]
                                    fixedWidth [7864-7885]
                                      operator [7864-7866] (": ")
                                      text [7866-7885] ("((a . 1) (b . qwe))")
                                    newLine [7885-7886]
                                    newLine [7886-7887]
                                    text [7887-7929] ("При чем точка нужна для специального типа ")
                                    italic [7929-7938]
                                      operator [7929-7930] ("/")
                                      text [7930-7937] ("symbols")
                                      operator [7937-7938] ("/")
                                    text [7938-7995] (". Если работает с реальными значениями то можно и без нее")
                                    newLine [7995-7996]
                                    newLine [7996-7997]
                                    srcBlock [7997-8094]
                                      blockHeader [7997-8019]
                                        keyword [7997-8019]
                                          text [7997-8008] ("#+begin_src")
                                          text [8008-8019] (" emacs-lisp")
                                      newLine [8019-8020]
                                      blockBody [8020-8084]
                                        text [8020-8084] ("(setq another-hashmap '((\\"a\\" \\"First elem\\") (\\"b\\" \\"Second elem\\")))")
                                      newLine [8084-8085]
                                      blockFooter [8085-8094]
                                        keyword [8085-8094]
                                          text [8085-8094] ("#+end_src")
                                    newLine [8094-8095]
                                headline [8095-8393]
                                    :level 4:
                                  title [8095-8126]
                                    operator [8095-8100] ("**** ")
                                    text [8100-8125] ("Получить элемент по ключу")
                                    newLine [8125-8126]
                                  section [8126-8393]
                                    srcBlock [8126-8190]
                                      blockHeader [8126-8148]
                                        keyword [8126-8148]
                                          text [8126-8137] ("#+begin_src")
                                          text [8137-8148] (" emacs-lisp")
                                      newLine [8148-8149]
                                      blockBody [8149-8180]
                                        text [8149-8180] ("(message \\"%s\\" (assoc 'a trees))")
                                      newLine [8180-8181]
                                      blockFooter [8181-8190]
                                        keyword [8181-8190]
                                          text [8181-8190] ("#+end_src")
                                    newLine [8190-8191]
                                    text [8191-8299] ("Ну и конечно возвращает оно кортеж..а чтобы получить элемент нужно использовать уже известную нам функцию - ")
                                    verbatim [8299-8304]
                                      operator [8299-8300] ("=")
                                      text [8300-8303] ("cdr")
                                      operator [8303-8304] ("=")
                                    newLine [8304-8305]
                                    newLine [8305-8306]
                                    srcBlock [8306-8376]
                                      blockHeader [8306-8328]
                                        keyword [8306-8328]
                                          text [8306-8317] ("#+begin_src")
                                          text [8317-8328] (" emacs-lisp")
                                      newLine [8328-8329]
                                      blockBody [8329-8366]
                                        text [8329-8366] ("(message \\"%s\\" (cdr (assoc 'a trees)))")
                                      newLine [8366-8367]
                                      blockFooter [8367-8376]
                                        keyword [8367-8376]
                                          text [8367-8376] ("#+end_src")
                                    newLine [8376-8377]
                                    keyword [8377-8388]
                                      text [8377-8387] ("#+RESULTS:")
                                      text [8387-8388] (" ")
                                    newLine [8388-8389]
                                    fixedWidth [8389-8392]
                                      operator [8389-8391] (": ")
                                      text [8391-8392] ("1")
                                    newLine [8392-8393]
                                headline [8393-8719]
                                    :level 4:
                                  title [8393-8427]
                                    operator [8393-8398] ("**** ")
                                    text [8398-8426] ("Получить элемент по значению")
                                    newLine [8426-8427]
                                  section [8427-8719]
                                    srcBlock [8427-8495]
                                      blockHeader [8427-8449]
                                        keyword [8427-8449]
                                          text [8427-8438] ("#+begin_src")
                                          text [8438-8449] (" emacs-lisp")
                                      newLine [8449-8450]
                                      blockBody [8450-8485]
                                        text [8450-8485] ("(message \\"%s\\" (rassoc \\"qwe\\" trees))")
                                      newLine [8485-8486]
                                      blockFooter [8486-8495]
                                        keyword [8486-8495]
                                          text [8486-8495] ("#+end_src")
                                    newLine [8495-8496]
                                    text [8496-8505] ("При этом ")
                                    italic [8505-8513]
                                      operator [8505-8506] ("/")
                                      text [8506-8512] ("rassoc")
                                      operator [8512-8513] ("/")
                                    text [8513-8554] (" работает и для строк и для чисел, а вот ")
                                    italic [8554-8561]
                                      operator [8554-8555] ("/")
                                      text [8555-8560] ("rassq")
                                      operator [8560-8561] ("/")
                                    text [8561-8578] (" только для чисел")
                                    newLine [8578-8579]
                                    newLine [8579-8580]
                                    srcBlock [8580-8696]
                                      blockHeader [8580-8602]
                                        keyword [8580-8602]
                                          text [8580-8591] ("#+begin_src")
                                          text [8591-8602] (" emacs-lisp")
                                      newLine [8602-8603]
                                      blockBody [8603-8686]
                                        text [8603-8686] ("(message \\"%s\\" (rassq \\"qwe\\" trees)) ;; nil\\n(message \\"%s\\" (rassq 1 trees)) ;; (a . 1)")
                                      newLine [8686-8687]
                                      blockFooter [8687-8696]
                                        keyword [8687-8696]
                                          text [8687-8696] ("#+end_src")
                                    newLine [8696-8697]
                                    keyword [8697-8708]
                                      text [8697-8707] ("#+RESULTS:")
                                      text [8707-8708] (" ")
                                    newLine [8708-8709]
                                    fixedWidth [8709-8718]
                                      operator [8709-8711] (": ")
                                      text [8711-8718] ("(a . 1)")
                                    newLine [8718-8719]
                                headline [8719-9053]
                                    :level 4:
                                  title [8719-8741]
                                    operator [8719-8724] ("**** ")
                                    text [8724-8740] ("Копирование мапы")
                                    newLine [8740-8741]
                                  section [8741-9053]
                                    srcBlock [8741-8979]
                                      blockHeader [8741-8763]
                                        keyword [8741-8763]
                                          text [8741-8752] ("#+begin_src")
                                          text [8752-8763] (" emacs-lisp")
                                      newLine [8763-8764]
                                      blockBody [8764-8969]
                                        text [8764-8969] ("  (setq needles-per-cluster\\n        '((2 . (\\"Austrian Pine\\" \\"Red Pine\\"))\\n          (3 . (\\"Pitch Pine\\"))\\n          (5 . (\\"White Pine\\"))))\\n  (setq copy (copy-alist needles-per-cluster))\\n  (message \\"%s\\" copy)")
                                      newLine [8969-8970]
                                      blockFooter [8970-8979]
                                        keyword [8970-8979]
                                          text [8970-8979] ("#+end_src")
                                    newLine [8979-8980]
                                    keyword [8980-8991]
                                      text [8980-8990] ("#+RESULTS:")
                                      text [8990-8991] (" ")
                                    newLine [8991-8992]
                                    fixedWidth [8992-9052]
                                      operator [8992-8994] (": ")
                                      text [8994-9052] ("((2 Austrian Pine Red Pine) (3 Pitch Pine) (5 White Pine))")
                                    newLine [9052-9053]
                                headline [9053-9232]
                                    :level 4:
                                  title [9053-9089]
                                    operator [9053-9058] ("**** ")
                                    text [9058-9088] ("Удаление всех записей по ключу")
                                    newLine [9088-9089]
                                  section [9089-9232]
                                    srcBlock [9089-9156]
                                      blockHeader [9089-9111]
                                        keyword [9089-9111]
                                          text [9089-9100] ("#+begin_src")
                                          text [9100-9111] (" emacs-lisp")
                                      newLine [9111-9112]
                                      blockBody [9112-9146]
                                        text [9112-9146] ("  (setq alist (list '(foo \\" alist)")
                                      newLine [9146-9147]
                                      blockFooter [9147-9156]
                                        keyword [9147-9156]
                                          text [9147-9156] ("#+end_src")
                                    newLine [9156-9157]
                                    keyword [9157-9168]
                                      text [9157-9167] ("#+RESULTS:")
                                      text [9167-9168] (" ")
                                    newLine [9168-9169]
                                    fixedWidth [9169-9204]
                                      operator [9169-9171] (": ")
                                      text [9171-9204] ("alist: ((foo 1) (bar 2) (lose 4))")
                                    newLine [9204-9205]
                                    fixedWidth [9205-9231]
                                      operator [9205-9207] (": ")
                                      text [9207-9231] (" new: ((bar 2) (lose 4))")
                                    newLine [9231-9232]
                                headline [9232-9729]
                                    :level 4:
                                  title [9232-9266]
                                    operator [9232-9237] ("**** ")
                                    text [9237-9265] ("Удаление записей по значению")
                                    newLine [9265-9266]
                                  section [9266-9729]
                                    srcBlock [9266-9612]
                                      blockHeader [9266-9288]
                                        keyword [9266-9288]
                                          text [9266-9277] ("#+begin_src")
                                          text [9277-9288] (" emacs-lisp")
                                      newLine [9288-9289]
                                      blockBody [9289-9602]
                                        text [9289-9602] ("  (setq alist2 '((foo . first) (bar . second) (foo2 . third) (qwe . five)))\\n  (setq new-alist (rassq-delete-all 'third alist2)) ;; меняет значение ?\\n  (message \\"%s\\" new-alist)\\n  (message (concat (format \\"alist: %s\\n\\" alist2)\\n                   (format \\"new: %s\\" new-alist)))\\n  ;; (message \\"%s\\" (rassq 'foo alist2))")
                                      newLine [9602-9603]
                                      blockFooter [9603-9612]
                                        keyword [9603-9612]
                                          text [9603-9612] ("#+end_src")
                                    newLine [9612-9613]
                                    keyword [9613-9624]
                                      text [9613-9623] ("#+RESULTS:")
                                      text [9623-9624] (" ")
                                    newLine [9624-9625]
                                    fixedWidth [9625-9677]
                                      operator [9625-9627] (": ")
                                      text [9627-9677] ("alist: ((foo . first) (bar . second) (qwe . five))")
                                    newLine [9677-9678]
                                    fixedWidth [9678-9728]
                                      operator [9678-9680] (": ")
                                      text [9680-9728] ("new: ((foo . first) (bar . second) (qwe . five))")
                                    newLine [9728-9729]
                            headline [9729-10397]
                                :level 3:
                              title [9729-9740]
                                operator [9729-9733] ("*** ")
                                text [9733-9739] ("Хешмап")
                                newLine [9739-9740]
                              section [9740-10397]
                                link [9740-9812]
                                    :linkType raw:
                                  operator [9740-9741] ("[")
                                  linkUrl [9741-9797]
                                    operator [9741-9742] ("[")
                                    text [9742-9796] ("htest-varp://ergoemacs.org/emacs/elisp_hash_table.html")
                                    operator [9796-9797] ("]")
                                  linkName [9797-9811]
                                    operator [9797-9798] ("[")
                                    text [9798-9810] ("Документация")
                                    operator [9810-9811] ("]")
                                  operator [9811-9812] ("]")
                                newLine [9812-9813]
                                newLine [9813-9814]
                                srcBlock [9814-10368]
                                  blockHeader [9814-9836]
                                    keyword [9814-9836]
                                      text [9814-9825] ("#+begin_src")
                                      text [9825-9836] (" emacs-lisp")
                                  newLine [9836-9837]
                                  blockBody [9837-10358]
                                    text [9837-10358] ("  (setq my-first-map #s(\\n                        hash-table\\n                        size 10\\n                        test equal\\n                        data (\\n                              python-mode \\"spam!\\"\\n                              go-mode \\"booo!1 terrible pointer\\"\\n                              org-mode \\"amma fluffy feature ;p\\"\\n                              )))\\n  (puthash 'js-mode \\"ugly language\\" my-first-map)\\n  (message \\"%s\\" (gethash 'python-mode my-first-map))\\n  (message \\"%s\\" (gethash 'js-mode my-first-map))")
                                  newLine [10358-10359]
                                  blockFooter [10359-10368]
                                    keyword [10359-10368]
                                      text [10359-10368] ("#+end_src")
                                newLine [10368-10369]
                                keyword [10369-10380]
                                  text [10369-10379] ("#+RESULTS:")
                                  text [10379-10380] (" ")
                                newLine [10380-10381]
                                fixedWidth [10381-10396]
                                  operator [10381-10383] (": ")
                                  text [10383-10396] ("ugly language")
                                newLine [10396-10397]
                            headline [10397-10535]
                                :level 3:
                              title [10397-10408]
                                operator [10397-10401] ("*** ")
                                text [10401-10407] ("Символ")
                                newLine [10407-10408]
                              section [10408-10535]
                                text [10408-10501] ("Тип данные соотвутствующий объекту с именем. Задаются символы с помощью 1 начальной кавычки. ")
                                verbatim [10501-10515]
                                  operator [10501-10502] ("=")
                                  text [10502-10514] ("'amma-symbol")
                                  operator [10514-10515] ("=")
                                newLine [10515-10516]
                                newLine [10516-10517]
                                keyword [10517-10534]
                                  text [10517-10534] ("#+CLOSE_{SPOILER}")
                                newLine [10534-10535]
                        headline [10535-14016]
                            :level 2:
                          title [10535-10546]
                            operator [10535-10538] ("** ")
                            text [10538-10545] ("Функции")
                            newLine [10545-10546]
                          section [10546-14016]
                            keyword [10546-10583]
                              text [10546-10583] ("#+START_{SPOILER} Читать про функции ")
                            text [10583-10584] (">")
                            newLine [10584-10585]
                            newLine [10585-10586]
                            headline [10586-11047]
                                :level 3:
                              title [10586-10609]
                                operator [10586-10590] ("*** ")
                                text [10590-10608] ("Объявление функций")
                                newLine [10608-10609]
                              section [10609-11047]
                                text [10609-10694] ("Функции принято комментировать, это позволяет смотреть документацию в автодополнении.")
                                newLine [10694-10695]
                                text [10695-10701] ("Вызов ")
                                verbatim [10701-10716]
                                  operator [10701-10702] ("=")
                                  text [10702-10715] ("(interactive)")
                                  operator [10715-10716] ("=")
                                text [10716-10823] (" означается что функция публичная и может быть взывана пользователем напрямую, либо через сочетание клавиш.")
                                newLine [10823-10824]
                                newLine [10824-10825]
                                srcBlock [10825-11014]
                                  blockHeader [10825-10847]
                                    keyword [10825-10847]
                                      text [10825-10836] ("#+begin_src")
                                      text [10836-10847] (" emacs-lisp")
                                  newLine [10847-10848]
                                  blockBody [10848-11004]
                                    text [10848-11004] ("  (defun hello (my-name)\\n    \\"This function will say hello for MY-NAME.\\"\\n    (interactive)\\n    (message (concat \\"Hello, I'am \\" my-name)))\\n\\n  (hello \\"Artur\\")")
                                  newLine [11004-11005]
                                  blockFooter [11005-11014]
                                    keyword [11005-11014]
                                      text [11005-11014] ("#+end_src")
                                newLine [11014-11015]
                                keyword [11015-11026]
                                  text [11015-11025] ("#+RESULTS:")
                                  text [11025-11026] (" ")
                                newLine [11026-11027]
                                fixedWidth [11027-11046]
                                  operator [11027-11029] (": ")
                                  text [11029-11046] ("Hello, I’am Artur")
                                newLine [11046-11047]
                            headline [11047-11328]
                                :level 3:
                              title [11047-11074]
                                operator [11047-11051] ("*** ")
                                text [11051-11073] ("Опицональные аргументы")
                                newLine [11073-11074]
                              section [11074-11328]
                                srcBlock [11074-11297]
                                  blockHeader [11074-11096]
                                    keyword [11074-11096]
                                      text [11074-11085] ("#+begin_src")
                                      text [11085-11096] (" emacs-lisp")
                                  newLine [11096-11097]
                                  blockBody [11097-11287]
                                    text [11097-11287] ("(defun my-super-optional-function (name &optional last-name patronymic)\\n  (message \\"%s %s %s\\" name (or last-name \\"\\") (or patronymic \\"\\")))\\n\\n(my-super-optional-function \\"Artur\\" nil \\"Proshkov\\")")
                                  newLine [11287-11288]
                                  blockFooter [11288-11297]
                                    keyword [11288-11297]
                                      text [11288-11297] ("#+end_src")
                                newLine [11297-11298]
                                keyword [11298-11309]
                                  text [11298-11308] ("#+RESULTS:")
                                  text [11308-11309] (" ")
                                newLine [11309-11310]
                                fixedWidth [11310-11327]
                                  operator [11310-11312] (": ")
                                  text [11312-11327] ("Artur  Proshkov")
                                newLine [11327-11328]
                            headline [11328-11645]
                                :level 3:
                              title [11328-11354]
                                operator [11328-11332] ("*** ")
                                text [11332-11353] ("Именованные аргументы")
                                newLine [11353-11354]
                              section [11354-11645]
                                srcBlock [11354-11603]
                                  blockHeader [11354-11376]
                                    keyword [11354-11376]
                                      text [11354-11365] ("#+begin_src")
                                      text [11365-11376] (" emacs-lisp")
                                  newLine [11376-11377]
                                  blockBody [11377-11593]
                                    text [11377-11593] ("(defun my-super-function-with-named-args (&rest args)\\n  (message \\"Name %s, middle name %s\\" (plist-get args :name) (plist-get args :middle-name)))\\n\\n  (my-super-function-with-named-args :name \\"One\\" :middle-name \\"Dude\\")")
                                  newLine [11593-11594]
                                  blockFooter [11594-11603]
                                    keyword [11594-11603]
                                      text [11594-11603] ("#+end_src")
                                newLine [11603-11604]
                                keyword [11604-11615]
                                  text [11604-11614] ("#+RESULTS:")
                                  text [11614-11615] (" ")
                                newLine [11615-11616]
                                fixedWidth [11616-11644]
                                  operator [11616-11618] (": ")
                                  text [11618-11644] ("Name One, middle name Dude")
                                newLine [11644-11645]
                            headline [11645-11829]
                                :level 3:
                              title [11645-11656]
                                operator [11645-11649] ("*** ")
                                text [11649-11655] ("Лямбды")
                                newLine [11655-11656]
                              section [11656-11829]
                                crossed [11656-11713]
                                  operator [11656-11657] ("+")
                                  text [11657-11712] ("Очевидно, лямбды нужны чтобы код можно было хуже читать")
                                  operator [11712-11713] ("+")
                                newLine [11713-11714]
                                newLine [11714-11715]
                                srcBlock [11715-11798]
                                  blockHeader [11715-11737]
                                    keyword [11715-11737]
                                      text [11715-11726] ("#+begin_src")
                                      text [11726-11737] (" emacs-lisp")
                                  newLine [11737-11738]
                                  blockBody [11738-11788]
                                    text [11738-11788] ("(funcall '(lambda () (message \\"I'am dirty func\\")))")
                                  newLine [11788-11789]
                                  blockFooter [11789-11798]
                                    keyword [11789-11798]
                                      text [11789-11798] ("#+end_src")
                                newLine [11798-11799]
                                keyword [11799-11810]
                                  text [11799-11809] ("#+RESULTS:")
                                  text [11809-11810] (" ")
                                newLine [11810-11811]
                                fixedWidth [11811-11828]
                                  operator [11811-11813] (": ")
                                  text [11813-11828] ("I’am dirty func")
                                newLine [11828-11829]
                            headline [11829-12406]
                                :level 3:
                              title [11829-11840]
                                operator [11829-11833] ("*** ")
                                text [11833-11839] ("Advice")
                                newLine [11839-11840]
                              section [11840-12406]
                                text [11840-11942] ("Адвайсы это прокаченные декораторы. Могут быть вызваны как до так и после вызова оригинальной функции.")
                                newLine [11942-11943]
                                newLine [11943-11944]
                                srcBlock [11944-12124]
                                  blockHeader [11944-11966]
                                    keyword [11944-11966]
                                      text [11944-11955] ("#+begin_src")
                                      text [11955-11966] (" emacs-lisp")
                                  newLine [11966-11967]
                                  blockBody [11967-12114]
                                    text [11967-12114] ("(defun my-increment (n)\\n  (+ n 1))\\n\\n(defun mux-5 (n)\\n  (* n 5))\\n\\n(advice-add 'my-increment :filter-return #'mux-5)\\n(message \\"%s\\" (my-increment 10))")
                                  newLine [12114-12115]
                                  blockFooter [12115-12124]
                                    keyword [12115-12124]
                                      text [12115-12124] ("#+end_src")
                                newLine [12124-12125]
                                keyword [12125-12136]
                                  text [12125-12135] ("#+RESULTS:")
                                  text [12135-12136] (" ")
                                newLine [12136-12137]
                                fixedWidth [12137-12141]
                                  operator [12137-12139] (": ")
                                  text [12139-12141] ("55")
                                newLine [12141-12142]
                                bold [12142-12184]
                                  operator [12142-12143] ("*")
                                  text [12143-12183] ("Пример адвайса после выполненеия функции")
                                  operator [12183-12184] ("*")
                                newLine [12184-12185]
                                newLine [12185-12186]
                                srcBlock [12186-12384]
                                  blockHeader [12186-12208]
                                    keyword [12186-12208]
                                      text [12186-12197] ("#+begin_src")
                                      text [12197-12208] (" emacs-lisp")
                                  newLine [12208-12209]
                                  blockBody [12209-12374]
                                    text [12209-12374] ("(defun my-first-func()\\n  (message \\"qweqwe\\"))\\n(my-first-func)\\n(defun my-adv()\\n  (message \\"advice called\\"))\\n(advice-add :after 'my-first-func #'my-adv)\\n(my-first-func)")
                                  newLine [12374-12375]
                                  blockFooter [12375-12384]
                                    keyword [12375-12384]
                                      text [12375-12384] ("#+end_src")
                                newLine [12384-12385]
                                keyword [12385-12396]
                                  text [12385-12395] ("#+RESULTS:")
                                  text [12395-12396] (" ")
                                newLine [12396-12397]
                                fixedWidth [12397-12405]
                                  operator [12397-12399] (": ")
                                  text [12399-12405] ("qweqwe")
                                newLine [12405-12406]
                            headline [12406-13613]
                                :level 3:
                              title [12406-12432]
                                operator [12406-12410] ("*** ")
                                text [12410-12431] ("Property list (plist)")
                                newLine [12431-12432]
                              section [12432-13613]
                                bold [12432-12452]
                                  operator [12432-12433] ("*")
                                  text [12433-12451] ("Установка и запись")
                                  operator [12451-12452] ("*")
                                newLine [12452-12453]
                                newLine [12453-12454]
                                srcBlock [12454-12678]
                                  blockHeader [12454-12476]
                                    keyword [12454-12476]
                                      text [12454-12465] ("#+begin_src")
                                      text [12465-12476] (" emacs-lisp")
                                  newLine [12476-12477]
                                  blockBody [12477-12668]
                                    text [12477-12668] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n(message \\"enabled: %s, another prop: %s, type: %s\\" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop) (type-of my-plist))")
                                  newLine [12668-12669]
                                  blockFooter [12669-12678]
                                    keyword [12669-12678]
                                      text [12669-12678] ("#+end_src")
                                newLine [12678-12679]
                                keyword [12679-12690]
                                  text [12679-12689] ("#+RESULTS:")
                                  text [12689-12690] (" ")
                                newLine [12690-12691]
                                fixedWidth [12691-12734]
                                  operator [12691-12693] (": ")
                                  text [12693-12734] ("enabled: t, another prop: hey, type: cons")
                                newLine [12734-12735]
                                newLine [12735-12736]
                                bold [12736-12747]
                                  operator [12736-12737] ("*")
                                  text [12737-12746] ("Изменение")
                                  operator [12746-12747] ("*")
                                newLine [12747-12748]
                                newLine [12748-12749]
                                srcBlock [12749-13002]
                                  blockHeader [12749-12771]
                                    keyword [12749-12771]
                                      text [12749-12760] ("#+begin_src")
                                      text [12760-12771] (" emacs-lisp")
                                  newLine [12771-12772]
                                  blockBody [12772-12992]
                                    text [12772-12992] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n\\n(plist-put my-plist  :another-prop \\"Wow, i was changed\\")\\n(message \\"enabled: %s, another prop: %s\\" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop))")
                                  newLine [12992-12993]
                                  blockFooter [12993-13002]
                                    keyword [12993-13002]
                                      text [12993-13002] ("#+end_src")
                                newLine [13002-13003]
                                bold [13003-13022]
                                  operator [13003-13004] ("*")
                                  text [13004-13021] ("Итерация по plist")
                                  operator [13021-13022] ("*")
                                newLine [13022-13023]
                                newLine [13023-13024]
                                srcBlock [13024-13401]
                                  blockHeader [13024-13046]
                                    keyword [13024-13046]
                                      text [13024-13035] ("#+begin_src")
                                      text [13035-13046] (" emacs-lisp")
                                  newLine [13046-13047]
                                  blockBody [13047-13391]
                                    text [13047-13391] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n\\n(setq res \\"res: \\")\\n(loop for (k v) on my-plist by 'cddr do\\n      (setq res (concat res (format \\"%s - %s\\" k v) \\"\\n\\")))\\n\\n;; (mapcar (lambda (k) (setq res (concat res (format \\"%s - \\" k ) \\"\\n\\"))) my-plist)\\n\\n\\n;; (dolist (p my-plist)\\n;;   (setq res (concat res (format \\"%s\\" p) \\"\\n\\")))\\n\\n(message res)")
                                  newLine [13391-13392]
                                  blockFooter [13392-13401]
                                    keyword [13392-13401]
                                      text [13392-13401] ("#+end_src")
                                newLine [13401-13402]
                                bold [13402-13430]
                                  operator [13402-13403] ("*")
                                  text [13403-13429] ("Удаление элемента из plist")
                                  operator [13429-13430] ("*")
                                newLine [13430-13431]
                                newLine [13431-13432]
                                srcBlock [13432-13563]
                                  blockHeader [13432-13454]
                                    keyword [13432-13454]
                                      text [13432-13443] ("#+begin_src")
                                      text [13443-13454] (" emacs-lisp")
                                  newLine [13454-13455]
                                  blockBody [13455-13553]
                                    text [13455-13553] ("(setq test '(:hi \\"there\\" :by \\"man!\\"))\\n\\n(setq test (map-delete test :hi))\\n\\n(message \\"res: %s\\" test)")
                                  newLine [13553-13554]
                                  blockFooter [13554-13563]
                                    keyword [13554-13563]
                                      text [13554-13563] ("#+end_src")
                                newLine [13563-13564]
                                keyword [13564-13575]
                                  text [13564-13574] ("#+RESULTS:")
                                  text [13574-13575] (" ")
                                newLine [13575-13576]
                                fixedWidth [13576-13593]
                                  operator [13576-13578] (": ")
                                  text [13578-13593] ("res: (:by man!)")
                                newLine [13593-13594]
                                newLine [13594-13595]
                                keyword [13595-13612]
                                  text [13595-13612] ("#+CLOSE_{SPOILER}")
                                newLine [13612-13613]
                            headline [13613-14016]
                                :level 3:
                              title [13613-13729]
                                operator [13613-13617] ("*** ")
                                link [13617-13728]
                                    :linkType raw:
                                  operator [13617-13618] ("[")
                                  linkUrl [13618-13700]
                                    operator [13618-13619] ("[")
                                    text [13619-13699] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html")
                                    operator [13699-13700] ("]")
                                  linkName [13700-13727]
                                    operator [13700-13701] ("[")
                                    text [13701-13726] ("Отложенный запуск функций")
                                    operator [13726-13727] ("]")
                                  operator [13727-13728] ("]")
                                newLine [13728-13729]
                              section [13729-14016]
                                srcBlock [13729-13842]
                                  blockHeader [13729-13751]
                                    keyword [13729-13751]
                                      text [13729-13740] ("#+begin_src")
                                      text [13740-13751] (" emacs-lisp")
                                  newLine [13751-13752]
                                  blockBody [13752-13832]
                                    text [13752-13832] ("(setq my-custom-timer (run-with-idle-timer 1 nil #'(lambda () (message \\"qwe\\"))))")
                                  newLine [13832-13833]
                                  blockFooter [13833-13842]
                                    keyword [13833-13842]
                                      text [13833-13842] ("#+end_src")
                                newLine [13842-13843]
                                keyword [13843-13854]
                                  text [13843-13853] ("#+RESULTS:")
                                  text [13853-13854] (" ")
                                newLine [13854-13855]
                                fixedWidth [13855-13915]
                                  operator [13855-13857] (": ")
                                  text [13857-13915] ("[nil 0 1 0 nil (lambda nil 'message \\"qwe\\") nil idle 0 nil]")
                                newLine [13915-13916]
                                newLine [13916-13917]
                                text [13917-13950] ("Отложенные функции можно отменять")
                                newLine [13950-13951]
                                newLine [13951-13952]
                                srcBlock [13952-14015]
                                  blockHeader [13952-13974]
                                    keyword [13952-13974]
                                      text [13952-13963] ("#+begin_src")
                                      text [13963-13974] (" emacs-lisp")
                                  newLine [13974-13975]
                                  blockBody [13975-14005]
                                    text [13975-14005] ("(cancel-timer my-custom-timer)")
                                  newLine [14005-14006]
                                  blockFooter [14006-14015]
                                    keyword [14006-14015]
                                      text [14006-14015] ("#+end_src")
                                newLine [14015-14016]
                        headline [14016-15406]
                            :level 2:
                          title [14016-14029]
                            operator [14016-14019] ("** ")
                            text [14019-14028] ("Операторы")
                            newLine [14028-14029]
                          section [14029-15406]
                            text [14029-14135] ("Орпеторы это точно такие же функции. Вынес в отдельную категорию т.к. в большинстве языков это инструкции.")
                            newLine [14135-14136]
                            keyword [14136-14161]
                              text [14136-14161] ("#+START_{SPOILER} Детали ")
                            text [14161-14162] (">")
                            newLine [14162-14163]
                            newLine [14163-14164]
                            headline [14164-14343]
                                :level 3:
                              title [14164-14180]
                                operator [14164-14168] ("*** ")
                                text [14168-14179] ("Switch case")
                                newLine [14179-14180]
                              section [14180-14343]
                                srcBlock [14180-14324]
                                  blockHeader [14180-14202]
                                    keyword [14180-14202]
                                      text [14180-14191] ("#+begin_src")
                                      text [14191-14202] (" emacs-lisp")
                                  newLine [14202-14203]
                                  blockBody [14203-14314]
                                    text [14203-14314] ("(setq test-var 'qwe)\\n(message \\"%s\\" (cond ((eq test-var 'q2e) 1)\\n       ((eq test-var 'oe) 2)\\n       (t \\"qwe\\")))")
                                  newLine [14314-14315]
                                  blockFooter [14315-14324]
                                    keyword [14315-14324]
                                      text [14315-14324] ("#+end_src")
                                newLine [14324-14325]
                                keyword [14325-14336]
                                  text [14325-14335] ("#+RESULTS:")
                                  text [14335-14336] (" ")
                                newLine [14336-14337]
                                fixedWidth [14337-14342]
                                  operator [14337-14339] (": ")
                                  text [14339-14342] ("qwe")
                                newLine [14342-14343]
                            headline [14343-14520]
                                :level 3:
                              title [14343-14353]
                                operator [14343-14347] ("*** ")
                                text [14347-14352] ("While")
                                newLine [14352-14353]
                              section [14353-14520]
                                srcBlock [14353-14502]
                                  blockHeader [14353-14375]
                                    keyword [14353-14375]
                                      text [14353-14364] ("#+begin_src")
                                      text [14364-14375] (" emacs-lisp")
                                  newLine [14375-14376]
                                  blockBody [14376-14492]
                                    text [14376-14492] ("(setq my-counter 0)\\n(while (< my-counter 12)\\n         (setq my-counter (+ my-counter 1)))\\n\\n(message \\"%s\\" my-counter)")
                                  newLine [14492-14493]
                                  blockFooter [14493-14502]
                                    keyword [14493-14502]
                                      text [14493-14502] ("#+end_src")
                                newLine [14502-14503]
                                keyword [14503-14514]
                                  text [14503-14513] ("#+RESULTS:")
                                  text [14513-14514] (" ")
                                newLine [14514-14515]
                                fixedWidth [14515-14519]
                                  operator [14515-14517] (": ")
                                  text [14517-14519] ("12")
                                newLine [14519-14520]
                            headline [14520-15037]
                                :level 3:
                              title [14520-14530]
                                operator [14520-14524] ("*** ")
                                text [14524-14529] ("Catch")
                                newLine [14529-14530]
                              section [14530-15037]
                                text [14530-14744] ("Просто вау, в фп есть try catch! Я действительно удивлен..даже в объектно ориетированых языках это вызывает проблемы..тем не менее..это 1 из вариантов прерываия цикла while (плохихи вариатов, как по мне, но все же)")
                                newLine [14744-14745]
                                newLine [14745-14746]
                                srcBlock [14746-15036]
                                  blockHeader [14746-14768]
                                    keyword [14746-14768]
                                      text [14746-14757] ("#+begin_src")
                                      text [14757-14768] (" emacs-lisp")
                                  newLine [14768-14769]
                                  blockBody [14769-15026]
                                    text [14769-15026] ("(setq my-counter 0)\\n\\n\\n(message \\"What is the messafe from catch? Oh this is message: %s\\" (catch 'result\\n  (while (< my-counter 22)\\n    (setq my-counter (+ my-counter 1))\\n    (if (> my-counter 5)\\n        (throw 'result \\"Amma result from catch block\\"))\\n    )))")
                                  newLine [15026-15027]
                                  blockFooter [15027-15036]
                                    keyword [15027-15036]
                                      text [15027-15036] ("#+end_src")
                                newLine [15036-15037]
                            headline [15037-15406]
                                :level 3:
                              title [15037-15048]
                                operator [15037-15041] ("*** ")
                                text [15041-15047] ("Return")
                                newLine [15047-15048]
                              section [15048-15406]
                                text [15048-15059] ("Работает в ")
                                bold [15059-15072]
                                  operator [15059-15060] ("*")
                                  text [15060-15071] ("emacs 27.1+")
                                  operator [15071-15072] ("*")
                                text [15072-15112] (". Позволяет прервать выполнение функции.")
                                newLine [15112-15113]
                                newLine [15113-15114]
                                srcBlock [15114-15370]
                                  blockHeader [15114-15136]
                                    keyword [15114-15136]
                                      text [15114-15125] ("#+begin_src")
                                      text [15125-15136] (" emacs-lisp")
                                  newLine [15136-15137]
                                  blockBody [15137-15360]
                                    text [15137-15360] ("(setq my-counter 0)\\n(cl-defun my-iterator()\\n  (while (< my-counter 12)\\n    (if (> my-counter 3)\\n        (return-from my-iterator)\\n      )\\n    (setq my-counter (+ my-counter 1)))\\n  )\\n\\n(my-iterator)\\n\\n(message \\"%s\\" my-counter)")
                                  newLine [15360-15361]
                                  blockFooter [15361-15370]
                                    keyword [15361-15370]
                                      text [15361-15370] ("#+end_src")
                                newLine [15370-15371]
                                keyword [15371-15382]
                                  text [15371-15381] ("#+RESULTS:")
                                  text [15381-15382] (" ")
                                newLine [15382-15383]
                                fixedWidth [15383-15386]
                                  operator [15383-15385] (": ")
                                  text [15385-15386] ("4")
                                newLine [15386-15387]
                                newLine [15387-15388]
                                keyword [15388-15405]
                                  text [15388-15405] ("#+CLOSE_{SPOILER}")
                                newLine [15405-15406]
                        headline [15406-20079]
                            :level 2:
                          title [15406-15432]
                            operator [15406-15409] ("** ")
                            text [15409-15431] ("Взаимодействие с emacs")
                            newLine [15431-15432]
                          section [15432-20079]
                            keyword [15432-15457]
                              text [15432-15457] ("#+START_{SPOILER} Детали ")
                            text [15457-15458] (">")
                            newLine [15458-15459]
                            newLine [15459-15460]
                            headline [15460-15543]
                                :level 3:
                              title [15460-15481]
                                operator [15460-15464] ("*** ")
                                text [15464-15480] ("Вставка в текста")
                                newLine [15480-15481]
                              section [15481-15543]
                                srcBlock [15481-15542]
                                  blockHeader [15481-15503]
                                    keyword [15481-15503]
                                      text [15481-15492] ("#+begin_src")
                                      text [15492-15503] (" emacs-lisp")
                                  newLine [15503-15504]
                                  blockBody [15504-15532]
                                    text [15504-15532] ("(insert \\"Hello\\" \\" \\" \\"World\\")")
                                  newLine [15532-15533]
                                  blockFooter [15533-15542]
                                    keyword [15533-15542]
                                      text [15533-15542] ("#+end_src")
                                newLine [15542-15543]
                            headline [15543-16126]
                                :level 3:
                              title [15543-15564]
                                operator [15543-15547] ("*** ")
                                text [15547-15563] ("Работа с буфером")
                                newLine [15563-15564]
                              section [15564-16126]
                                headline [15564-15739]
                                    :level 4:
                                  title [15564-15604]
                                    operator [15564-15569] ("**** ")
                                    text [15569-15603] ("Программное создание нового буфера")
                                    newLine [15603-15604]
                                  section [15604-15739]
                                    srcBlock [15604-15738]
                                      blockHeader [15604-15626]
                                        keyword [15604-15626]
                                          text [15604-15615] ("#+begin_src")
                                          text [15615-15626] (" emacs-lisp")
                                      newLine [15626-15627]
                                      blockBody [15627-15728]
                                        text [15627-15728] ("  (switch-to-buffer-other-window \\"*my-first-buffer*\\")\\n  (insert \\"Congratulations! I'am a new buffer\\")")
                                      newLine [15728-15729]
                                      blockFooter [15729-15738]
                                        keyword [15729-15738]
                                          text [15729-15738] ("#+end_src")
                                    newLine [15738-15739]
                                headline [15739-15807]
                                    :level 4:
                                  title [15739-15759]
                                    operator [15739-15744] ("**** ")
                                    text [15744-15758] ("Очистка буфера")
                                    newLine [15758-15759]
                                  section [15759-15807]
                                    srcBlock [15759-15806]
                                      blockHeader [15759-15781]
                                        keyword [15759-15781]
                                          text [15759-15770] ("#+begin_src")
                                          text [15770-15781] (" emacs-lisp")
                                      newLine [15781-15782]
                                      blockBody [15782-15796]
                                        text [15782-15796] ("(erase-buffer)")
                                      newLine [15796-15797]
                                      blockFooter [15797-15806]
                                        keyword [15797-15806]
                                          text [15797-15806] ("#+end_src")
                                    newLine [15806-15807]
                                headline [15807-16126]
                                    :level 4:
                                  title [15807-15831]
                                    operator [15807-15812] ("**** ")
                                    text [15812-15830] ("Интерактивный ввод")
                                    newLine [15830-15831]
                                  section [15831-16126]
                                    srcBlock [15831-16114]
                                      blockHeader [15831-15853]
                                        keyword [15831-15853]
                                          text [15831-15842] ("#+begin_src")
                                          text [15842-15853] (" emacs-lisp")
                                      newLine [15853-15854]
                                      blockBody [15854-16104]
                                        text [15854-16104] ("  ;; (read-from-minibuffer \\"Enter your name: \\")\\n  (let ((your-name (read-from-minibuffer \\"Enter your name: \\")))\\n      (switch-to-buffer-other-window \\"*Your personal info\\")\\n  (erase-buffer)\\n  (insert (format \\"Hello %s!\\" your-name))\\n  (other-window 1))")
                                      newLine [16104-16105]
                                      blockFooter [16105-16114]
                                        keyword [16105-16114]
                                          text [16105-16114] ("#+end_src")
                                    newLine [16114-16115]
                                    keyword [16115-16125]
                                      text [16115-16125] ("#+RESULTS:")
                                    newLine [16125-16126]
                            headline [16126-17526]
                                :level 3:
                              title [16126-16147]
                                operator [16126-16130] ("*** ")
                                text [16130-16146] ("Replace в буфере")
                                newLine [16146-16147]
                              section [16147-17526]
                                srcBlock [16147-16571]
                                  blockHeader [16147-16169]
                                    keyword [16147-16169]
                                      text [16147-16158] ("#+begin_src")
                                      text [16158-16169] (" emacs-lisp")
                                  newLine [16169-16170]
                                  blockBody [16170-16561]
                                    text [16170-16561] ("  (defun detect-bad-boys ()\\n    (setq lesson-list '(\\"Buzova\\" \\"Volodin\\" \\"Pupin\\"))\\n  \\n    (defun mark-as-bad (name)\\n      (insert (format \\"Bad boy %s \\n\\" name)))\\n  \\n    (switch-to-buffer-other-window \\"*lisp lesson*\\")\\n    (mapcar 'mark-as-bad lesson-list)\\n    (goto-char (point-min))\\n    (while (search-forward \\"Bad\\")\\n      (replace-match \\"Awful\\"))\\n    (other-window 1)\\n    )\\n  (detect-bad-boys)")
                                  newLine [16561-16562]
                                  blockFooter [16562-16571]
                                    keyword [16562-16571]
                                      text [16562-16571] ("#+end_src")
                                newLine [16571-16572]
                                bold [16572-16583]
                                  operator [16572-16573] ("*")
                                  text [16573-16582] ("goto-char")
                                  operator [16582-16583] ("*")
                                text [16583-16584] (" ")
                                list [16584-16632]
                                    :unordered:
                                    :level 0:
                                  listItem [16584-16616]
                                    title [16584-16616]
                                      operator [16584-16586] ("- ")
                                      text [16586-16615] ("переход к конкретному символу")
                                      newLine [16615-16616]
                                  listItem [16616-16632]
                                    title [16616-16632]
                                      operator [16616-16618] ("- ")
                                      text [16618-16631] ("начало буфера")
                                      newLine [16631-16632]
                                bold [16632-16643]
                                  operator [16632-16633] ("*")
                                  text [16633-16642] ("point-min")
                                  operator [16642-16643] ("*")
                                text [16643-16644] (" ")
                                headline [16644-17526]
                                    :level 3:
                                  title [16644-16678]
                                    operator [16644-16648] ("*** ")
                                    text [16648-16677] ("Добавление свойств для текста")
                                    newLine [16677-16678]
                                  section [16678-17526]
                                    italic [16678-16730]
                                      operator [16678-16679] ("/")
                                      text [16679-16729] ("Перед этим необходимо запустить предыдущую функцию")
                                      operator [16729-16730] ("/")
                                    newLine [16730-16731]
                                    newLine [16731-16732]
                                    srcBlock [16732-17209]
                                      blockHeader [16732-16754]
                                        keyword [16732-16754]
                                          text [16732-16743] ("#+begin_src")
                                          text [16743-16754] (" emacs-lisp")
                                      newLine [16754-16755]
                                      blockBody [16755-17199]
                                        text [16755-17199] ("  ;; (detect-bad-boys)\\n  \\n  \\n  (defun boldify-bad-boys ()\\n    (switch-to-buffer-other-window \\"*lisp lesson*\\")\\n    (goto-char (point-min))\\n    (while (re-search-forward \\"Awful boy \\\\(.+\\\\)\\" nil t)\\n      (message (format \\"Its %s\\" (match-beginning 1)))\\n      (add-text-properties (match-beginning 1)\\n                           (match-end 1)\\n                           (list 'face 'bold-italic)))\\n    ;; (other-window 1)\\n    )\\n  \\n  (boldify-bad-boys)")
                                      newLine [17199-17200]
                                      blockFooter [17200-17209]
                                        keyword [17200-17209]
                                          text [17200-17209] ("#+end_src")
                                    newLine [17209-17210]
                                    keyword [17210-17221]
                                      text [17210-17220] ("#+RESULTS:")
                                      text [17220-17221] (" ")
                                    newLine [17221-17222]
                                    text [17222-17246] ("Про сумасшедшие регекспы")
                                    newLine [17246-17247]
                                    newLine [17247-17248]
                                    quoteBlock [17248-17525]
                                      blockHeader [17248-17261]
                                        keyword [17248-17261]
                                          text [17248-17261] ("#+begin_quote")
                                      newLine [17261-17262]
                                      blockBody [17262-17513]
                                        text [17262-17300] (";; The regular expression is \\"Bonjour ")
                                        keyword [17300-17303]
                                          text [17300-17303] ("\\\\(.")
                                        text [17303-17304] ("+")
                                        keyword [17304-17322]
                                          text [17304-17308] ("\\\\)!\\"")
                                          text [17308-17322] (" and it reads:")
                                        newLine [17322-17323]
                                        text [17323-17352] (";; the string \\"Bonjour \\", and")
                                        newLine [17352-17353]
                                        text [17353-17391] (";; a group of           | this is the ")
                                        keyword [17391-17398]
                                          text [17391-17393] ("\\\\(")
                                          text [17393-17398] (" ... ")
                                        keyword [17398-17410]
                                          text [17398-17400] ("\\\\)")
                                          text [17400-17410] (" construct")
                                        newLine [17410-17411]
                                        text [17411-17450] (";;   any character      | this is the .")
                                        newLine [17450-17451]
                                        text [17451-17490] (";;   possibly repeated  | this is the +")
                                        newLine [17490-17491]
                                        text [17491-17513] (";; and the \\"!\\" string.")
                                      newLine [17513-17514]
                                      blockFooter [17514-17525]
                                        keyword [17514-17525]
                                          text [17514-17525] ("#+end_quote")
                                    newLine [17525-17526]
                            headline [17526-18181]
                                :level 3:
                              title [17526-17548]
                                operator [17526-17530] ("*** ")
                                text [17530-17547] ("Создание кнопочки")
                                newLine [17547-17548]
                              section [17548-18181]
                                text [17548-17607] ("Даннный метод создает кнопку над текстом с позиции от 1 до ")
                                text [17607-17610] ("10.")
                                newLine [17610-17611]
                                newLine [17611-17612]
                                srcBlock [17612-17920]
                                  blockHeader [17612-17634]
                                    keyword [17612-17634]
                                      text [17612-17623] ("#+begin_src")
                                      text [17623-17634] (" emacs-lisp")
                                  newLine [17634-17635]
                                  blockBody [17635-17910]
                                    text [17635-17910] ("(defun butest-varon-pressed (button)\\n  (message (format \\"Butest-varon pressed!\\")))\\n\\n(define-butest-varon-type 'custom-button\\n  'action 'butest-varon-pressed\\n  'follow-link t\\n  'help-echo \\"Click Butest-varon\\"\\n  'help-args \\"test\\")\\n\\n(make-butest-varon 1 10 :type 'custom-button)")
                                  newLine [17910-17911]
                                  blockFooter [17911-17920]
                                    keyword [17911-17920]
                                      text [17911-17920] ("#+end_src")
                                newLine [17920-17921]
                                text [17921-17981] ("Данная функция вставляет кнопку под текущей позицей каретки.")
                                newLine [17981-17982]
                                newLine [17982-17983]
                                srcBlock [17983-18129]
                                  blockHeader [17983-18005]
                                    keyword [17983-18005]
                                      text [17983-17994] ("#+begin_src")
                                      text [17994-18005] (" emacs-lisp")
                                  newLine [18005-18006]
                                  blockBody [18006-18119]
                                    text [18006-18119] ("(insert-butest-varon \\"Press me\\"\\n               'action (lambda (_arg) (print \\"You are press the butest-varon!\\")))")
                                  newLine [18119-18120]
                                  blockFooter [18120-18129]
                                    keyword [18120-18129]
                                      text [18120-18129] ("#+end_src")
                                newLine [18129-18130]
                                keyword [18130-18141]
                                  text [18130-18140] ("#+RESULTS:")
                                  text [18140-18141] (" ")
                                newLine [18141-18142]
                                fixedWidth [18142-18180]
                                  operator [18142-18144] (": ")
                                  text [18144-18180] ("#<overlay from 1 to 10 in elisp.org>")
                                newLine [18180-18181]
                            headline [18181-18293]
                                :level 3:
                              title [18181-18206]
                                operator [18181-18185] ("*** ")
                                text [18185-18205] ("Чтение из completion")
                                newLine [18205-18206]
                              section [18206-18293]
                                srcBlock [18206-18292]
                                  blockHeader [18206-18228]
                                    keyword [18206-18228]
                                      text [18206-18217] ("#+begin_src")
                                      text [18217-18228] (" emacs-lisp")
                                  newLine [18228-18229]
                                  blockBody [18229-18282]
                                    text [18229-18282] ("(completing-read \\"Choose one: \\" '(\\"foo\\" \\"bar\\" \\"baz\\"))")
                                  newLine [18282-18283]
                                  blockFooter [18283-18292]
                                    keyword [18283-18292]
                                      text [18283-18292] ("#+end_src")
                                newLine [18292-18293]
                            headline [18293-18448]
                                :level 3:
                              title [18293-18319]
                                operator [18293-18297] ("*** ")
                                text [18297-18318] ("Пользовательский ввод")
                                newLine [18318-18319]
                              section [18319-18448]
                                srcBlock [18319-18408]
                                  blockHeader [18319-18341]
                                    keyword [18319-18341]
                                      text [18319-18330] ("#+begin_src")
                                      text [18330-18341] (" emacs-lisp")
                                  newLine [18341-18342]
                                  blockBody [18342-18398]
                                    text [18342-18398] ("(message \\"U say: %s\\" (read-string \\"Say me something: \\"))")
                                  newLine [18398-18399]
                                  blockFooter [18399-18408]
                                    keyword [18399-18408]
                                      text [18399-18408] ("#+end_src")
                                newLine [18408-18409]
                                keyword [18409-18420]
                                  text [18409-18419] ("#+RESULTS:")
                                  text [18419-18420] (" ")
                                newLine [18420-18421]
                                fixedWidth [18421-18447]
                                  operator [18421-18423] (": ")
                                  text [18423-18447] ("U say: Ну че тут скажешь")
                                newLine [18447-18448]
                            headline [18448-18634]
                                :level 3:
                              title [18448-18480]
                                operator [18448-18452] ("*** ")
                                text [18452-18479] ("Работа с выделенным текстом")
                                newLine [18479-18480]
                              section [18480-18634]
                                headline [18480-18531]
                                    :level 4:
                                  title [18480-18514]
                                    operator [18480-18485] ("**** ")
                                    text [18485-18513] ("Проверка что что-то выделено")
                                    newLine [18513-18514]
                                  section [18514-18531]
                                    verbatim [18514-18530]
                                      operator [18514-18515] ("=")
                                      text [18515-18529] ("(use-region-p)")
                                      operator [18529-18530] ("=")
                                    newLine [18530-18531]
                                headline [18531-18634]
                                    :level 4:
                                  title [18531-18562]
                                    operator [18531-18536] ("**** ")
                                    text [18536-18561] ("Получить выделенный текст")
                                    newLine [18561-18562]
                                  section [18562-18634]
                                    srcBlock [18562-18633]
                                      blockHeader [18562-18584]
                                        keyword [18562-18584]
                                          text [18562-18573] ("#+begin_src")
                                          text [18573-18584] (" emacs-lisp")
                                      newLine [18584-18585]
                                      blockBody [18585-18623]
                                        text [18585-18623] ("(regionp (buffer-substring start end))")
                                      newLine [18623-18624]
                                      blockFooter [18624-18633]
                                        keyword [18624-18633]
                                          text [18624-18633] ("#+end_src")
                                    newLine [18633-18634]
                            headline [18634-18853]
                                :level 3:
                              title [18634-18680]
                                operator [18634-18638] ("*** ")
                                text [18638-18679] ("Конвертация символа в строку (ну и назад)")
                                newLine [18679-18680]
                              section [18680-18853]
                                srcBlock [18680-18809]
                                  blockHeader [18680-18702]
                                    keyword [18680-18702]
                                      text [18680-18691] ("#+begin_src")
                                      text [18691-18702] (" emacs-lisp")
                                  newLine [18702-18703]
                                  blockBody [18703-18799]
                                    text [18703-18799] ("(symbol-name 'something) ;; Символ в строку\\n(intern (symbol-name 'something)) ;; Строка в символ")
                                  newLine [18799-18800]
                                  blockFooter [18800-18809]
                                    keyword [18800-18809]
                                      text [18800-18809] ("#+end_src")
                                newLine [18809-18810]
                                keyword [18810-18821]
                                  text [18810-18820] ("#+RESULTS:")
                                  text [18820-18821] (" ")
                                newLine [18821-18822]
                                fixedWidth [18822-18833]
                                  operator [18822-18824] (": ")
                                  text [18824-18833] ("something")
                                newLine [18833-18834]
                                newLine [18834-18835]
                                keyword [18835-18852]
                                  text [18835-18852] ("#+CLOSE_{SPOILER}")
                                newLine [18852-18853]
                            headline [18853-19934]
                                :level 3:
                              title [18853-18865]
                                operator [18853-18857] ("*** ")
                                text [18857-18864] ("Overlay")
                                newLine [18864-18865]
                              section [18865-19934]
                                text [18865-19045] ("Overlay это очень крутая тема. Он позволяет рендерить текст который не изменяет контент реального буфера. Это может быть полезно для показа подсказок, дебага, расчитанных значений.")
                                newLine [19045-19046]
                                newLine [19046-19047]
                                headline [19047-19196]
                                    :level 4:
                                  title [19047-19084]
                                    operator [19047-19052] ("**** ")
                                    text [19052-19083] ("Создание оверлея в конце строки")
                                    newLine [19083-19084]
                                  section [19084-19196]
                                    srcBlock [19084-19195]
                                      blockHeader [19084-19106]
                                        keyword [19084-19106]
                                          text [19084-19095] ("#+begin_src")
                                          text [19095-19106] (" emacs-lisp")
                                      newLine [19106-19107]
                                      blockBody [19107-19185]
                                        text [19107-19185] ("(setq my-first-overlay (make-overlay (line-end-position) (line-end-position)))")
                                      newLine [19185-19186]
                                      blockFooter [19186-19195]
                                        keyword [19186-19195]
                                          text [19186-19195] ("#+end_src")
                                    newLine [19195-19196]
                                headline [19196-19491]
                                    :level 4:
                                  title [19196-19234]
                                    operator [19196-19201] ("**** ")
                                    text [19201-19233] ("Курсор заходит за предел оверлея")
                                    newLine [19233-19234]
                                  section [19234-19491]
                                    text [19234-19364] ("В моем случае курсор выходил за предел оверлея. Решается весьма просто: вставляемый в оверлей текст необходимо наделить свойством ")
                                    verbatim [19364-19375]
                                      operator [19364-19365] ("=")
                                      text [19365-19374] ("'cursor t")
                                      operator [19374-19375] ("=")
                                    newLine [19375-19376]
                                    newLine [19376-19377]
                                    srcBlock [19377-19490]
                                      blockHeader [19377-19399]
                                        keyword [19377-19399]
                                          text [19377-19388] ("#+begin_src")
                                          text [19388-19399] (" emacs-lisp")
                                      newLine [19399-19400]
                                      blockBody [19400-19480]
                                        text [19400-19480] ("(setq my-popup-message (propertize popup-message 'face 'blamer--face 'cursor t))")
                                      newLine [19480-19481]
                                      blockFooter [19481-19490]
                                        keyword [19481-19490]
                                          text [19481-19490] ("#+end_src")
                                    newLine [19490-19491]
                                headline [19491-19801]
                                    :level 4:
                                  title [19491-19522]
                                    operator [19491-19496] ("**** ")
                                    text [19496-19521] ("Изменение свойств overlay")
                                    newLine [19521-19522]
                                  section [19522-19801]
                                    srcBlock [19522-19800]
                                      blockHeader [19522-19544]
                                        keyword [19522-19544]
                                          text [19522-19533] ("#+begin_src")
                                          text [19533-19544] (" emacs-lisp")
                                      newLine [19544-19545]
                                      blockBody [19545-19790]
                                        text [19545-19790] ("    (overlay-put blamer--current-overlay 'after-string my-popup-message)\\n    (overlay-put blamer--current-overlay 'intangible t)\\n    (overlay-put blamer--current-overlay 'face 'bold)\\n    (overlay-put blamer--current-overlay 'cursor-intangible t)")
                                      newLine [19790-19791]
                                      blockFooter [19791-19800]
                                        keyword [19791-19800]
                                          text [19791-19800] ("#+end_src")
                                    newLine [19800-19801]
                                headline [19801-19934]
                                    :level 4:
                                  title [19801-19837]
                                    operator [19801-19806] ("**** ")
                                    text [19806-19836] ("Удаление существующего оверлея")
                                    newLine [19836-19837]
                                  section [19837-19934]
                                    srcBlock [19837-19933]
                                      blockHeader [19837-19859]
                                        keyword [19837-19859]
                                          text [19837-19848] ("#+begin_src")
                                          text [19848-19859] (" emacs-lisp")
                                      newLine [19859-19860]
                                      blockBody [19860-19923]
                                        text [19860-19923] ("(if my-first-overlay\\n        (delete-overlay my-first-overlay))")
                                      newLine [19923-19924]
                                      blockFooter [19924-19933]
                                        keyword [19924-19933]
                                          text [19924-19933] ("#+end_src")
                                    newLine [19933-19934]
                            headline [19934-20079]
                                :level 3:
                              title [19934-19971]
                                operator [19934-19938] ("*** ")
                                text [19938-19965] ("Создание своего minor-mode ")
                                tagList [19965-19970]
                                  operator [19965-19966] (":")
                                  text [19966-19969] ("WIP")
                                  operator [19969-19970] (":")
                                newLine [19970-19971]
                              section [19971-20079]
                                link [19971-20078]
                                    :linkType raw:
                                  operator [19971-19972] ("[")
                                  linkUrl [19972-20063]
                                    operator [19972-19973] ("[")
                                    text [19973-20062] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html")
                                    operator [20062-20063] ("]")
                                  linkName [20063-20077]
                                    operator [20063-20064] ("[")
                                    text [20064-20076] ("Документация")
                                    operator [20076-20077] ("]")
                                  operator [20077-20078] ("]")
                                newLine [20078-20079]
                        headline [20079-20468]
                            :level 2:
                          title [20079-20098]
                            operator [20079-20082] ("** ")
                            text [20082-20097] ("Работа с датами")
                            newLine [20097-20098]
                          section [20098-20468]
                            link [20098-20223]
                                :linkType raw:
                              operator [20098-20099] ("[")
                              linkUrl [20099-20195]
                                operator [20099-20100] ("[")
                                text [20100-20194] ("htest-varps://stackoverflow.com/questions/4242012/how-do-i-add-dates-in-emacs-using-emacs-lisp")
                                operator [20194-20195] ("]")
                              linkName [20195-20222]
                                operator [20195-20196] ("[")
                                text [20196-20221] ("Ага, любимый стаковерфлоу")
                                operator [20221-20222] ("]")
                              operator [20222-20223] ("]")
                            newLine [20223-20224]
                            newLine [20224-20225]
                            srcBlock [20225-20315]
                              blockHeader [20225-20247]
                                keyword [20225-20247]
                                  text [20225-20236] ("#+begin_src")
                                  text [20236-20247] (" emacs-lisp")
                              newLine [20247-20248]
                              blockBody [20248-20305]
                                text [20248-20305] ("(setq t3 (time-subtract (current-time) (days-to-time 2)))")
                              newLine [20305-20306]
                              blockFooter [20306-20315]
                                keyword [20306-20315]
                                  text [20306-20315] ("#+end_src")
                            newLine [20315-20316]
                            table [20316-20346]
                              tableRow [20316-20345]
                                operator [20316-20317] ("|")
                                tableCell [20317-20324]
                                  text [20317-20324] (" 24939 ")
                                operator [20324-20325] ("|")
                                tableCell [20325-20331]
                                  text [20325-20331] (" 1255 ")
                                operator [20331-20332] ("|")
                                tableCell [20332-20340]
                                  text [20332-20340] (" 721279 ")
                                operator [20340-20341] ("|")
                                tableCell [20341-20344]
                                  text [20341-20344] (" 0 ")
                                operator [20344-20345] ("|")
                              newLine [20345-20346]
                            srcBlock [20346-20434]
                              blockHeader [20346-20368]
                                keyword [20346-20368]
                                  text [20346-20357] ("#+begin_src")
                                  text [20357-20368] (" emacs-lisp")
                              newLine [20368-20369]
                              blockBody [20369-20424]
                                text [20369-20424] ("\\n(message \\"%s\\" (/ (float-time (time-since t3)) (* 60)))")
                              newLine [20424-20425]
                              blockFooter [20425-20434]
                                keyword [20425-20434]
                                  text [20425-20434] ("#+end_src")
                            newLine [20434-20435]
                            keyword [20435-20446]
                              text [20435-20445] ("#+RESULTS:")
                              text [20445-20446] (" ")
                            newLine [20446-20447]
                            fixedWidth [20447-20467]
                              operator [20447-20449] (": ")
                              text [20449-20467] ("2940.0710639333333")
                            newLine [20467-20468]
                        headline [20468-21494]
                            :level 2:
                          title [20468-20478]
                            operator [20468-20471] ("** ")
                            text [20471-20477] ("Regexp")
                            newLine [20477-20478]
                          section [20478-21494]
                            headline [20478-21139]
                                :level 3:
                              title [20478-20490]
                                operator [20478-20482] ("*** ")
                                text [20482-20489] ("Примеры")
                                newLine [20489-20490]
                              section [20490-21139]
                                text [20490-20535] ("Просто кучка примеров из разработанного мной ")
                                link [20535-20591]
                                    :linkType raw:
                                  operator [20535-20536] ("[")
                                  linkUrl [20536-20582]
                                    operator [20536-20537] ("[")
                                    text [20537-20581] ("htest-varps://github.com/Artawower/turbo-log")
                                    operator [20581-20582] ("]")
                                  linkName [20582-20590]
                                    operator [20582-20583] ("[")
                                    text [20583-20589] ("пакета")
                                    operator [20589-20590] ("]")
                                  operator [20590-20591] ("]")
                                text [20591-20767] (". Регекспы весьма похожи на то что представлено в других языках. Сложно лишь работать с интерполяцией строк (неочевидна работа с большим количеством слешей в исполняемом коде.)")
                                newLine [20767-20768]
                                newLine [20768-20769]
                                srcBlock [20769-20860]
                                  blockHeader [20769-20791]
                                    keyword [20769-20791]
                                      text [20769-20780] ("#+begin_src")
                                      text [20780-20791] (" emacs-lisp")
                                  newLine [20791-20792]
                                  blockBody [20792-20850]
                                    text [20792-20850] ("(string-match \\"^\\\\(\\\\)*\\\\(return\\\\)\\" \\"  return {\\n  name: 2\\n}\\")")
                                  newLine [20850-20851]
                                  blockFooter [20851-20860]
                                    keyword [20851-20860]
                                      text [20851-20860] ("#+end_src")
                                newLine [20860-20861]
                                keyword [20861-20872]
                                  text [20861-20871] ("#+RESULTS:")
                                  text [20871-20872] (" ")
                                newLine [20872-20873]
                                fixedWidth [20873-20876]
                                  operator [20873-20875] (": ")
                                  text [20875-20876] ("0")
                                newLine [20876-20877]
                                newLine [20877-20878]
                                newLine [20878-20879]
                                srcBlock [20879-20999]
                                  blockHeader [20879-20901]
                                    keyword [20879-20901]
                                      text [20879-20890] ("#+begin_src")
                                      text [20890-20901] (" emacs-lisp")
                                  newLine [20901-20902]
                                  blockBody [20902-20989]
                                    text [20902-20989] ("(replace-regexp-in-string \\"*=[[:blank:]]*.+\\" \\"\\" \\"    this.myVariable = somethingElse;\\")")
                                  newLine [20989-20990]
                                  blockFooter [20990-20999]
                                    keyword [20990-20999]
                                      text [20990-20999] ("#+end_src")
                                newLine [20999-21000]
                                srcBlock [21000-21107]
                                  blockHeader [21000-21022]
                                    keyword [21000-21022]
                                      text [21000-21011] ("#+begin_src")
                                      text [21011-21022] (" emacs-lisp")
                                  newLine [21022-21023]
                                  blockBody [21023-21097]
                                    text [21023-21097] ("(replace-regexp-in-string \\"\\\\(const\\\\|let\\\\|public\\\\|protected\\\\|private\\\\|var\\\\)")
                                  newLine [21097-21098]
                                  blockFooter [21098-21107]
                                    keyword [21098-21107]
                                      text [21098-21107] ("#+end_src")
                                newLine [21107-21108]
                                keyword [21108-21119]
                                  text [21108-21118] ("#+RESULTS:")
                                  text [21118-21119] (" ")
                                newLine [21119-21120]
                                fixedWidth [21120-21138]
                                  operator [21120-21122] (": ")
                                  text [21122-21138] ("iable = userName")
                                newLine [21138-21139]
                            headline [21139-21494]
                                :level 3:
                              title [21139-21165]
                                operator [21139-21143] ("*** ")
                                text [21143-21164] ("Regexp с группировкой")
                                newLine [21164-21165]
                              section [21165-21494]
                                srcBlock [21165-21305]
                                  blockHeader [21165-21187]
                                    keyword [21165-21187]
                                      text [21165-21176] ("#+begin_src")
                                      text [21176-21187] (" emacs-lisp")
                                  newLine [21187-21188]
                                  blockBody [21188-21295]
                                    text [21188-21295] ("(concat \\"^(?\\\\(?1:\\\\) [^s]\\n]+\\\\)\\"\\n          \\"s\\\\(?3:-[0-9]-[0-9]\\\\)\\"\\n          \\"s\\\\(?4:\\\\{2\\\\}:[0-9]\\\\{2\\\\}:[0-9]\\\\)\\")")
                                  newLine [21295-21296]
                                  blockFooter [21296-21305]
                                    keyword [21296-21305]
                                      text [21296-21305] ("#+end_src")
                                newLine [21305-21306]
                                srcBlock [21306-21475]
                                  blockHeader [21306-21328]
                                    keyword [21306-21328]
                                      text [21306-21317] ("#+begin_src")
                                      text [21317-21328] (" emacs-lisp")
                                  newLine [21328-21329]
                                  blockBody [21329-21465]
                                    text [21329-21465] ("(setq test-string \\"feature/VW-221\\")\\n(string-match \\"\\\\(?1:+/\\\\)\\\\(?2:VW-[0-9]+\\\\)\\" test-string)\\n(message \\"res \\" (match-string 1 test-string))")
                                  newLine [21465-21466]
                                  blockFooter [21466-21475]
                                    keyword [21466-21475]
                                      text [21466-21475] ("#+end_src")
                                newLine [21475-21476]
                                keyword [21476-21487]
                                  text [21476-21486] ("#+RESULTS:")
                                  text [21486-21487] (" ")
                                newLine [21487-21488]
                                fixedWidth [21488-21493]
                                  operator [21488-21490] (": ")
                                  text [21490-21493] ("res")
                                newLine [21493-21494]
                        headline [21494-21594]
                            :level 2:
                          title [21494-21514]
                            operator [21494-21497] ("** ")
                            text [21497-21513] ("Стандартные хуки")
                            newLine [21513-21514]
                          section [21514-21594]
                            link [21514-21593]
                                :linkType raw:
                              operator [21514-21515] ("[")
                              linkUrl [21515-21572]
                                operator [21515-21516] ("[")
                                text [21516-21571] ("htest-varps://runebook.dev/ru/docs/elisp/standard-hooks")
                                operator [21571-21572] ("]")
                              linkName [21572-21592]
                                operator [21572-21573] ("[")
                                text [21573-21591] ("Просто смотри сюда")
                                operator [21591-21592] ("]")
                              operator [21592-21593] ("]")
                            newLine [21593-21594]
                        headline [21594-22325]
                            :level 2:
                          title [21594-21610]
                            operator [21594-21597] ("** ")
                            text [21597-21609] ("Custom modes")
                            newLine [21609-21610]
                          section [21610-22325]
                            headline [21610-22325]
                                :level 3:
                              title [21610-21625]
                                operator [21610-21614] ("*** ")
                                text [21614-21624] ("Minor mode")
                                newLine [21624-21625]
                              section [21625-22325]
                                text [21625-21732] ("Для того чтобы сделать свой minor mode достаточно его объявить и описать логику включения/выключений режима")
                                newLine [21732-21733]
                                newLine [21733-21734]
                                srcBlock [21734-22079]
                                  blockHeader [21734-21756]
                                    keyword [21734-21756]
                                      text [21734-21745] ("#+begin_src")
                                      text [21745-21756] (" emacs-lisp")
                                  newLine [21756-21757]
                                  blockBody [21757-22069]
                                    text [21757-22069] (";;;###autoload\\n(define-minor-mode wakatime-ui-mode\\n  \\"Wakatime ui mode. Add time track to doom modeline.\\nTODO:\\nAdd support for other modeline in future.\\"\\n  :init-value nil\\n  :global t\\n  :lighter nil\\n  :group 'wakatime-ui\\n  (if wakatime-ui-mode\\n      (wakatime-ui--watch-time)\\n    (wakatime-ui--stop-watch-time)))")
                                  newLine [22069-22070]
                                  blockFooter [22070-22079]
                                    keyword [22070-22079]
                                      text [22070-22079] ("#+end_src")
                                newLine [22079-22080]
                                text [22080-22084] ("Где:")
                                newLine [22084-22085]
                                newLine [22085-22086]
                                verbatim [22086-22098]
                                  operator [22086-22087] ("=")
                                  text [22087-22097] ("init-value")
                                  operator [22097-22098] ("=")
                                text [22098-22099] (" ")
                                list [22099-22236]
                                    :unordered:
                                    :level 0:
                                  listItem [22099-22123]
                                    title [22099-22123]
                                      operator [22099-22101] ("- ")
                                      text [22101-22122] ("значение по умолчанию")
                                      newLine [22122-22123]
                                  listItem [22123-22179]
                                    title [22123-22179]
                                      operator [22123-22125] ("- ")
                                      text [22125-22178] ("должен ли быть вызван глобальный мод перед локальным?")
                                      newLine [22178-22179]
                                  listItem [22179-22236]
                                    title [22179-22236]
                                      operator [22179-22181] ("- ")
                                      text [22181-22235] ("определяет что отображать в modeline когда мод включен")
                                      newLine [22235-22236]
                                verbatim [22236-22244]
                                  operator [22236-22237] ("=")
                                  text [22237-22243] ("global")
                                  operator [22243-22244] ("=")
                                text [22244-22245] (" ")
                                verbatim [22245-22254]
                                  operator [22245-22246] ("=")
                                  text [22246-22253] ("lighter")
                                  operator [22253-22254] ("=")
                                text [22254-22255] (" ")
                                headline [22255-22325]
                                    :level 2:
                                  title [22255-22265]
                                    operator [22255-22258] ("** ")
                                    text [22258-22264] ("Window")
                                    newLine [22264-22265]
                                  section [22265-22325]
                                    headline [22265-22325]
                                        :level 3:
                                      title [22265-22302]
                                        operator [22265-22269] ("*** ")
                                        text [22269-22301] ("Получение ширины текущего экрана")
                                        newLine [22301-22302]
                                      section [22302-22325]
                                        verbatim [22302-22324]
                                          operator [22302-22303] ("=")
                                          text [22303-22323] ("(window-total-width)")
                                          operator [22323-22324] ("=")
                                        newLine [22324-22325]
                        headline [22325-22826]
                            :level 2:
                          title [22325-22361]
                            operator [22325-22328] ("** ")
                            text [22328-22360] ("Асинхронное исполнение. Process.")
                            newLine [22360-22361]
                          section [22361-22826]
                            text [22361-22393] ("Создание асинхронного процесаа (")
                            link [22393-22504]
                                :linkType raw:
                              operator [22393-22394] ("[")
                              linkUrl [22394-22487]
                                operator [22394-22395] ("[")
                                text [22395-22486] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html")
                                operator [22486-22487] ("]")
                              linkName [22487-22503]
                                operator [22487-22488] ("[")
                                text [22488-22502] ("подробнее тут)")
                                operator [22502-22503] ("]")
                              operator [22503-22504] ("]")
                            newLine [22504-22505]
                            newLine [22505-22506]
                            srcBlock [22506-22755]
                              blockHeader [22506-22528]
                                keyword [22506-22528]
                                  text [22506-22517] ("#+begin_src")
                                  text [22517-22528] (" emacs-lisp")
                              newLine [22528-22529]
                              blockBody [22529-22745]
                                text [22529-22745] ("(setq process (start-process\\n                    \\"WakatimeUI\\"\\n                    wakatime-ui--buffer\\n                    (wakatime-find-binary)\\n                    (plist-get wakatime-ui--command-args :today-time)))")
                              newLine [22745-22746]
                              blockFooter [22746-22755]
                                keyword [22746-22755]
                                  text [22746-22755] ("#+end_src")
                            newLine [22755-22756]
                            text [22756-22790] ("Чтение выходных данных из процесса")
                            newLine [22790-22791]
                            newLine [22791-22792]
                            srcBlock [22792-22825]
                              blockHeader [22792-22814]
                                keyword [22792-22814]
                                  text [22792-22803] ("#+begin_src")
                                  text [22803-22814] (" emacs-lisp")
                              newLine [22814-22815]
                              newLine [22815-22816]
                              blockFooter [22816-22825]
                                keyword [22816-22825]
                                  text [22816-22825] ("#+end_src")
                            newLine [22825-22826]
                        headline [22826-23455]
                            :level 2:
                          title [22826-22837]
                            operator [22826-22829] ("** ")
                            text [22829-22836] ("Keymaps")
                            newLine [22836-22837]
                          section [22837-23455]
                            headline [22837-23455]
                                :level 3:
                              title [22837-22864]
                                operator [22837-22841] ("*** ")
                                text [22841-22863] ("Создание своего keymap")
                                newLine [22863-22864]
                              section [22864-23455]
                                srcBlock [22864-23443]
                                  blockHeader [22864-22881]
                                    keyword [22864-22881]
                                      text [22864-22875] ("#+begin_src")
                                      text [22875-22881] (" elisp")
                                  newLine [22881-22882]
                                  blockBody [22882-23433]
                                    text [22882-23433] ("(with-current-buffer \\"*Messages*\\"\\n  (read-only-mode -1)\\n  (erase-buffer))\\n\\n(setq my-mode-map (make-sparse-keymap))\\n(define-key my-mode-map (kbd \\"C-c C-'\\") 'my-mode-cmd1)\\n(define-key my-mode-map (kbd \\"C-c C-b\\") 'my-mode-cmd2)\\n(define-key my-mode-map (kbd \\"C-c C-c\\") 'my-mode-cmd3)\\n(define-key my-mode-map (kbd \\"<mouse-1>\\") 'my-mode-cmd4)\\n;; by convention, major mode's keys should begin with the form C-c C-‹key›\\n\\n;; (dolist (m my-mode-map)\\n;;   (message \\"key: %s\\" m))\\n\\n\\n\\n\\n\\n(map-keymap '(lambda (v g)\\n               (message \\"%s: %s\\" v g)) my-mode-map)")
                                  newLine [23433-23434]
                                  blockFooter [23434-23443]
                                    keyword [23434-23443]
                                      text [23434-23443] ("#+end_src")
                                newLine [23443-23444]
                                keyword [23444-23454]
                                  text [23444-23454] ("#+RESULTS:")
                                newLine [23454-23455]
                        headline [23455-28590]
                            :level 2:
                            :id  elisp-macros:
                          title [23455-23464]
                            operator [23455-23458] ("** ")
                            text [23458-23463] ("Macro")
                            newLine [23463-23464]
                          section [23464-28590]
                            propertyDrawer [23464-23500]
                              property [23464-23476]
                                text [23464-23476] (":PROPERTIES:")
                              newLine [23476-23477]
                              property [23477-23494]
                                text [23477-23481] (":ID:")
                                text [23481-23494] (" elisp-macros")
                              newLine [23494-23495]
                              property [23495-23500]
                                text [23495-23500] (":END:")
                            newLine [23500-23501]
                            text [23501-23511] ("Подробнее ")
                            link [23511-23595]
                                :linkType raw:
                              operator [23511-23512] ("[")
                              linkUrl [23512-23589]
                                operator [23512-23513] ("[")
                                text [23513-23588] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html")
                                operator [23588-23589] ("]")
                              linkName [23589-23594]
                                operator [23589-23590] ("[")
                                text [23590-23593] ("тут")
                                operator [23593-23594] ("]")
                              operator [23594-23595] ("]")
                            text [23595-23596] (".")
                            newLine [23596-23597]
                            newLine [23597-23598]
                            headline [23598-23772]
                                :level 3:
                              title [23598-23617]
                                operator [23598-23602] ("*** ")
                                text [23602-23616] ("Простой макрос")
                                newLine [23616-23617]
                              section [23617-23772]
                                srcBlock [23617-23754]
                                  blockHeader [23617-23639]
                                    keyword [23617-23639]
                                      text [23617-23628] ("#+begin_src")
                                      text [23628-23639] (" emacs-lisp")
                                  newLine [23639-23640]
                                  blockBody [23640-23744]
                                    text [23640-23744] ("(defmacro inc (var)\\n  (list 'setq var (list '1+ var)))\\n\\n(setq test-var 10)\\n(message \\"%s\\" (inc test-var))")
                                  newLine [23744-23745]
                                  blockFooter [23745-23754]
                                    keyword [23745-23754]
                                      text [23745-23754] ("#+end_src")
                                newLine [23754-23755]
                                keyword [23755-23766]
                                  text [23755-23765] ("#+RESULTS:")
                                  text [23765-23766] (" ")
                                newLine [23766-23767]
                                fixedWidth [23767-23771]
                                  operator [23767-23769] (": ")
                                  text [23769-23771] ("11")
                                newLine [23771-23772]
                            headline [23772-23947]
                                :level 3:
                              title [23772-23791]
                                operator [23772-23776] ("*** ")
                                text [23776-23790] ("Изучить макрос")
                                newLine [23790-23791]
                              section [23791-23947]
                                text [23791-23829] ("Macroexpand - показывает вывод макроса")
                                newLine [23829-23830]
                                newLine [23830-23831]
                                srcBlock [23831-23908]
                                  blockHeader [23831-23853]
                                    keyword [23831-23853]
                                      text [23831-23842] ("#+begin_src")
                                      text [23842-23853] (" emacs-lisp")
                                  newLine [23853-23854]
                                  blockBody [23854-23898]
                                    text [23854-23898] ("(message \\"%s\\" (macroexpand '(inc test-var)))")
                                  newLine [23898-23899]
                                  blockFooter [23899-23908]
                                    keyword [23899-23908]
                                      text [23899-23908] ("#+end_src")
                                newLine [23908-23909]
                                keyword [23909-23920]
                                  text [23909-23919] ("#+RESULTS:")
                                  text [23919-23920] (" ")
                                newLine [23920-23921]
                                fixedWidth [23921-23946]
                                  operator [23921-23923] (": ")
                                  text [23923-23946] ("(setq test-var (1+ tt))")
                                newLine [23946-23947]
                            headline [23947-24286]
                                :level 3:
                              title [23947-23971]
                                operator [23947-23951] ("*** ")
                                text [23951-23970] ("Цепочка из макросов")
                                newLine [23970-23971]
                              section [23971-24286]
                                text [23971-24053] ("Macroexpand отображает только первый макрос, чтобы вызвать всю цепочку используем ")
                                verbatim [24053-24070]
                                  operator [24053-24054] ("=")
                                  text [24054-24069] ("macroexpand-all")
                                  operator [24069-24070] ("=")
                                newLine [24070-24071]
                                newLine [24071-24072]
                                srcBlock [24072-24231]
                                  blockHeader [24072-24094]
                                    keyword [24072-24094]
                                      text [24072-24083] ("#+begin_src")
                                      text [24083-24094] (" emacs-lisp")
                                  newLine [24094-24095]
                                  blockBody [24095-24221]
                                    text [24095-24221] ("(defmacro inc2 (var1 var2)\\n    (list 'progn (list 'inc var1) (list 'inc var2)))\\n\\n\\n(message \\"%s\\" (macroexpand-all '(inc2 r s)))")
                                  newLine [24221-24222]
                                  blockFooter [24222-24231]
                                    keyword [24222-24231]
                                      text [24222-24231] ("#+end_src")
                                newLine [24231-24232]
                                keyword [24232-24243]
                                  text [24232-24242] ("#+RESULTS:")
                                  text [24242-24243] (" ")
                                newLine [24243-24244]
                                fixedWidth [24244-24285]
                                  operator [24244-24246] (": ")
                                  text [24246-24285] ("(progn (setq r (1+ r)) (setq s (1+ s)))")
                                newLine [24285-24286]
                            headline [24286-24494]
                                :level 3:
                              title [24286-24328]
                                operator [24286-24290] ("*** ")
                                text [24290-24327] ("Пример с более сложными конструкциями")
                                newLine [24327-24328]
                              section [24328-24494]
                                srcBlock [24328-24475]
                                  blockHeader [24328-24350]
                                    keyword [24328-24350]
                                      text [24328-24339] ("#+begin_src")
                                      text [24339-24350] (" emacs-lisp")
                                  newLine [24350-24351]
                                  blockBody [24351-24465]
                                    text [24351-24465] ("(defmacro t-becomes-nil (var)\\n  \`(if (eq ,var t)\\n       (setq ,var nil)))\\n\\n(message \\"%s\\" (t-becomes-nil test-var))")
                                  newLine [24465-24466]
                                  blockFooter [24466-24475]
                                    keyword [24466-24475]
                                      text [24466-24475] ("#+end_src")
                                newLine [24475-24476]
                                keyword [24476-24487]
                                  text [24476-24486] ("#+RESULTS:")
                                  text [24486-24487] (" ")
                                newLine [24487-24488]
                                fixedWidth [24488-24493]
                                  operator [24488-24490] (": ")
                                  text [24490-24493] ("nil")
                                newLine [24493-24494]
                            headline [24494-25603]
                                :level 3:
                              title [24494-24532]
                                operator [24494-24498] ("*** ")
                                text [24498-24531] ("Динамическое получение переменной")
                                newLine [24531-24532]
                              section [24532-25603]
                                indent [24532-24533] (" ")
                                link [24533-24654]
                                    :linkType network:
                                  operator [24533-24534] ("[")
                                  linkUrl [24534-24636]
                                    operator [24534-24535] ("[")
                                    text [24535-24635] ("https://stackoverflow.com/questions/24433035/combining-two-variables-into-one-function-name-in-macro")
                                    operator [24635-24636] ("]")
                                  linkName [24636-24653]
                                    operator [24636-24637] ("[")
                                    text [24637-24652] ("Подсмотрено тут")
                                    operator [24652-24653] ("]")
                                  operator [24653-24654] ("]")
                                newLine [24654-24655]
                                indent [24655-24656] (" ")
                                text [24656-24673] ("Чертовая магия 😮")
                                newLine [24673-24674]
                                newLine [24674-24675]
                                srcBlock [24675-24913]
                                  blockHeader [24675-24697]
                                    keyword [24675-24697]
                                      text [24675-24686] ("#+begin_src")
                                      text [24686-24697] (" emacs-lisp")
                                  newLine [24697-24698]
                                  blockBody [24698-24903]
                                    text [24698-24903] ("(setq my-custom-variable \\"Hello, amma variable\\")\\n\\n(defmacro get-with-prefix (var-name)\\n  \`(symbol-value (intern (concatenate 'string \\"my-custom\\" \\"-\\" (symbol-name ',var-name)))))\\n\\n(get-with-prefix variable)")
                                  newLine [24903-24904]
                                  blockFooter [24904-24913]
                                    keyword [24904-24913]
                                      text [24904-24913] ("#+end_src")
                                newLine [24913-24914]
                                keyword [24914-24925]
                                  text [24914-24924] ("#+RESULTS:")
                                  text [24924-24925] (" ")
                                newLine [24925-24926]
                                fixedWidth [24926-24948]
                                  operator [24926-24928] (": ")
                                  text [24928-24948] ("Hello, amma variable")
                                newLine [24948-24949]
                                newLine [24949-24950]
                                text [24950-25116] ("А теперь из plist, если нет - то из глобального скоупа, это еще большая магия. Да, наверное такое не стоит использовать в реальных проектах, но как же руки чешутся 😍")
                                newLine [25116-25117]
                                newLine [25117-25118]
                                srcBlock [25118-25567]
                                  blockHeader [25118-25140]
                                    keyword [25118-25140]
                                      text [25118-25129] ("#+begin_src")
                                      text [25129-25140] (" emacs-lisp")
                                  newLine [25140-25141]
                                  blockBody [25141-25557]
                                    text [25141-25557] ("(setq my-custom-variable \\"Hello, amma variable\\")\\n\\n(setq my-plist-with-prop '(:custom-variable nil :test t))\\n\\n(defmacro get-with-prefix (my-plist var-name)\\n  \`(or (plist-get ,my-plist (symbol-value (intern (concatenate 'string \\":\\" (symbol-name ',var-name)))))\\n       (symbol-value (intern (concatenate 'string \\"my\\" \\"-\\" (symbol-name ',var-name))))))\\n\\n(message \\"%s\\" (get-with-prefix my-plist-with-prop custom-variable))")
                                  newLine [25557-25558]
                                  blockFooter [25558-25567]
                                    keyword [25558-25567]
                                      text [25558-25567] ("#+end_src")
                                newLine [25567-25568]
                                keyword [25568-25579]
                                  text [25568-25578] ("#+RESULTS:")
                                  text [25578-25579] (" ")
                                newLine [25579-25580]
                                fixedWidth [25580-25602]
                                  operator [25580-25582] (": ")
                                  text [25582-25602] ("Hello, amma variable")
                                newLine [25602-25603]
                            headline [25603-26752]
                                :level 3:
                              title [25603-25640]
                                operator [25603-25607] ("*** ")
                                text [25607-25629] ("Передача тела (@body) ")
                                tagList [25629-25639]
                                  operator [25629-25630] (":")
                                  text [25630-25638] ("noexport")
                                  operator [25638-25639] (":")
                                newLine [25639-25640]
                              section [25640-26752]
                                text [25640-25794] ("Пожалуй самая впечатлаяющая фича (имхо, без нее смысл в макросах бы отпал). Макрос склеивает результаты выполнения функций (подумал для org-mode самое то)")
                                newLine [25794-25795]
                                newLine [25795-25796]
                                srcBlock [25796-26125]
                                  blockHeader [25796-25818]
                                    keyword [25796-25818]
                                      text [25796-25807] ("#+begin_src")
                                      text [25807-25818] (" emacs-lisp")
                                  newLine [25818-25819]
                                  blockBody [25819-26115]
                                    text [25819-26115] ("(setq test-var 0)\\n(defmacro for (var from init to final do &rest body)\\n  \`(let ((,var ,init))\\n     (while (<= ,var ,final)\\n       ,@body\\n       (setq ,var (1+ ,var)))))\\n\\n\\n(for j from 0 to 4 do\\n     (setq test-var (+ test-var j))\\n     (setq test-var (/ test-var 2)))\\n\\n(message \\"HAVA: %s\\" test-var)")
                                  newLine [26115-26116]
                                  blockFooter [26116-26125]
                                    keyword [26116-26125]
                                      text [26116-26125] ("#+end_src")
                                newLine [26125-26126]
                                keyword [26126-26137]
                                  text [26126-26136] ("#+RESULTS:")
                                  text [26136-26137] (" ")
                                newLine [26137-26138]
                                fixedWidth [26138-26147]
                                  operator [26138-26140] (": ")
                                  text [26140-26147] ("HAVA: 3")
                                newLine [26147-26148]
                                newLine [26148-26149]
                                newLine [26149-26150]
                                headline [26150-26752]
                                    :level 4:
                                  title [26150-26173]
                                    operator [26150-26155] ("**** ")
                                    text [26155-26162] ("Failed ")
                                    tagList [26162-26172]
                                      operator [26162-26163] (":")
                                      text [26163-26171] ("noexport")
                                      operator [26171-26172] (":")
                                    newLine [26172-26173]
                                  section [26173-26752]
                                    text [26173-26242] ("Пример макроса, чтобы наглядно видеть в орге какая функция что делает")
                                    newLine [26242-26243]
                                    newLine [26243-26244]
                                    srcBlock [26244-26568]
                                      blockHeader [26244-26266]
                                        keyword [26244-26266]
                                          text [26244-26255] ("#+begin_src")
                                          text [26255-26266] (" emacs-lisp")
                                      newLine [26266-26267]
                                      blockBody [26267-26558]
                                        text [26267-26558] ("(defmacro pretty-log (&rest body)\\n\\n  (let ((res (concat (make-string 80 ?-) \\"\\n\\")))\\n    (dolist (f body)\\n      (setq res (concat res (format \\"[%s]: %s\\n\\" f (eval f)))))\\n    (message res)))\\n\\n(pretty-log (+ 1 12)\\n            (- 44 22)\\n            (+ (/ 12 2) (* 33 4))\\n            (setq ttt 12))")
                                      newLine [26558-26559]
                                      blockFooter [26559-26568]
                                        keyword [26559-26568]
                                          text [26559-26568] ("#+end_src")
                                    newLine [26568-26569]
                                    keyword [26569-26580]
                                      text [26569-26579] ("#+RESULTS:")
                                      text [26579-26580] (" ")
                                    newLine [26580-26581]
                                    fixedWidth [26581-26663]
                                      operator [26581-26583] (": ")
                                      text [26583-26663] ("--------------------------------------------------------------------------------")
                                    newLine [26663-26664]
                                    fixedWidth [26664-26680]
                                      operator [26664-26666] (": ")
                                      text [26666-26680] ("[(+ 1 12)]: 13")
                                    newLine [26680-26681]
                                    fixedWidth [26681-26698]
                                      operator [26681-26683] (": ")
                                      text [26683-26698] ("[(- 44 22)]: 22")
                                    newLine [26698-26699]
                                    fixedWidth [26699-26729]
                                      operator [26699-26701] (": ")
                                      text [26701-26729] ("[(+ (/ 12 2) (* 33 4))]: 138")
                                    newLine [26729-26730]
                                    fixedWidth [26730-26751]
                                      operator [26730-26732] (": ")
                                      text [26732-26751] ("[(setq ttt 12)]: 12")
                                    newLine [26751-26752]
                            headline [26752-28590]
                                :level 3:
                              title [26752-26840]
                                operator [26752-26756] ("*** ")
                                text [26756-26829] ("Модификация plist через список динамических аргументов как в use-package ")
                                tagList [26829-26839]
                                  operator [26829-26830] (":")
                                  text [26830-26838] ("noexport")
                                  operator [26838-26839] (":")
                                newLine [26839-26840]
                              section [26840-28590]
                                srcBlock [26840-28365]
                                  blockHeader [26840-26862]
                                    keyword [26840-26862]
                                      text [26840-26851] ("#+begin_src")
                                      text [26851-26862] (" emacs-lisp")
                                  newLine [26862-26863]
                                  blockBody [26863-28355]
                                    text [26863-28355] ("(setq res \\"\\")\\n(setq test-alist\\n      '((js-mode (:loggers '(\\"hi there\\") :msg-format-template \\"Hi\\" :argument-divider \\"|\\"))\\n        (typescript-mode (:loggers '(\\"another on\\", \\"and me\\") :msg-format-template \\"bee\\"))\\n        ))\\n\\n(defmacro turbo-log-configure (&rest configs)\\n  (let* ((strategy (or (plist-get configs :strategy) 'replace))\\n         (excluded-keys '(:modes :strategy))\\n         (modes (plist-get configs :modes))\\n         current-config)\\n\\n    (dolist (k excluded-keys)\\n      (setq configs (map-delete configs k)))\\n\\n    (dolist (mode modes)\\n      (unless (assoc mode test-alist)\\n        (push \`(,mode nil) test-alist))\\n\\n      (setq current-config (car (cdr-safe (assoc mode test-alist))))\\n\\n      (if (eq strategy 'replace)\\n          (setq current-config configs)\\n\\n        (loop for (k v) on configs by 'cddr do\\n              (if current-config\\n                  (plist-put current-config k v)\\n                (setq current-config \`(,k ,v)))))\\n\\n      (message \\"QQQ: %s\\" configs)\\n      (if (assq mode test-alist)\\n          (setcdr (assq mode test-alist)\\n                  \`(,current-config))\\n        \`(push '(,mode '(,current-config)) ,test-alist))\\n      )))\\n\\n(turbo-log-configure\\n :modes (typescript-mode js2-mode js-mode)\\n ;; :modes (typescript-mode j-mode)\\n ;; :modes (js-mode)\\n :strategy replace\\n\\n :loggers (\\"console.print\\" \\"console.dbg\\")\\n :msg-format-template \\"\\"HELLO WORLD: %s\\"\\")\\n\\n(message \\"-------------------------------------------------------\\")\\n(message \\"%s\\" (pp test-alist))")
                                  newLine [28355-28356]
                                  blockFooter [28356-28365]
                                    keyword [28356-28365]
                                      text [28356-28365] ("#+end_src")
                                newLine [28365-28366]
                                keyword [28366-28377]
                                  text [28366-28376] ("#+RESULTS:")
                                  text [28376-28377] (" ")
                                newLine [28377-28378]
                                fixedWidth [28378-28391]
                                  operator [28378-28380] (": ")
                                  text [28380-28391] ("((mode nil)")
                                newLine [28391-28392]
                                fixedWidth [28392-28403]
                                  operator [28392-28394] (": ")
                                  text [28394-28403] (" (js-mode")
                                newLine [28403-28404]
                                fixedWidth [28404-28417]
                                  operator [28404-28406] (": ")
                                  text [28406-28417] ("  (:loggers")
                                newLine [28417-28418]
                                fixedWidth [28418-28436]
                                  operator [28418-28420] (": ")
                                  text [28420-28436] ("   '(\\"hi there\\")")
                                newLine [28436-28437]
                                fixedWidth [28437-28469]
                                  operator [28437-28439] (": ")
                                  text [28439-28469] ("   :msg-format-template \\"Hi\\"))")
                                newLine [28469-28470]
                                fixedWidth [28470-28489]
                                  operator [28470-28472] (": ")
                                  text [28472-28489] (" (typescript-mode")
                                newLine [28489-28490]
                                fixedWidth [28490-28503]
                                  operator [28490-28492] (": ")
                                  text [28492-28503] ("  (:loggers")
                                newLine [28503-28504]
                                fixedWidth [28504-28540]
                                  operator [28504-28506] (": ")
                                  text [28506-28540] ("   (\\"console.print\\" \\"console.dbg\\")")
                                newLine [28540-28541]
                                fixedWidth [28541-28589]
                                  operator [28541-28543] (": ")
                                  text [28543-28589] ("   :msg-format-template \\"\\"HELLO WORLD: %s\\"\\")))")
                                newLine [28589-28590]
        headline [28590-29022]
            :level 1:
          title [28590-28615]
            operator [28590-28592] ("* ")
            text [28592-28614] ("Создание своего пакета")
            newLine [28614-28615]
          section [28615-29022]
            headline [28615-28777]
                :level 2:
              title [28615-28645]
                operator [28615-28618] ("** ")
                text [28618-28644] ("Проверка ошибок компиляции")
                newLine [28644-28645]
              section [28645-28777]
                srcBlock [28645-28776]
                  blockHeader [28645-28661]
                    keyword [28645-28661]
                      text [28645-28656] ("#+begin_src")
                      text [28656-28661] (" bash")
                  newLine [28661-28662]
                  blockBody [28662-28766]
                    text [28662-28766] ("emacs -Q --batch     --eval '(setq byte-compile-error-on-warn t)'     -f batch-byte-compile turbo-log.el")
                  newLine [28766-28767]
                  blockFooter [28767-28776]
                    keyword [28767-28776]
                      text [28767-28776] ("#+end_src")
                newLine [28776-28777]
            headline [28777-28840]
                :level 2:
              title [28777-28791]
                operator [28777-28780] ("** ")
                text [28780-28790] ("Contribute")
                newLine [28790-28791]
              section [28791-28840]
                link [28791-28839]
                    :linkType raw:
                  operator [28791-28792] ("[")
                  linkUrl [28792-28838]
                    operator [28792-28793] ("[")
                    text [28793-28837] ("htest-varps://github.com/leotaku/elisp-check")
                    operator [28837-28838] ("]")
                  operator [28838-28839] ("]")
                newLine [28839-28840]
            headline [28840-29022]
                :level 2:
              title [28840-28846]
                operator [28840-28843] ("** ")
                text [28843-28845] ("CI")
                newLine [28845-28846]
              section [28846-29022]
                link [28846-28955]
                    :linkType raw:
                  operator [28846-28847] ("[")
                  linkUrl [28847-28931]
                    operator [28847-28848] ("[")
                    text [28848-28930] ("htest-varps://github.com/a13/reverse-im.el/blob/master/.github/workflows/check.yml")
                    operator [28930-28931] ("]")
                  linkName [28931-28954]
                    operator [28931-28932] ("[")
                    text [28932-28953] ("Пример github actions")
                    operator [28953-28954] ("]")
                  operator [28954-28955] ("]")
                newLine [28955-28956]
                link [28956-29021]
                    :linkType raw:
                  operator [28956-28957] ("[")
                  linkUrl [28957-29003]
                    operator [28957-28958] ("[")
                    text [28958-29002] ("htest-varps://github.com/leotaku/elisp-check")
                    operator [29002-29003] ("]")
                  linkName [29003-29020]
                    operator [29003-29004] ("[")
                    text [29004-29019] ("Про elisp check")
                    operator [29019-29020] ("]")
                  operator [29020-29021] ("]")
                newLine [29021-29022]
        headline [29022-29389]
            :level 1:
          title [29022-29030]
            operator [29022-29024] ("* ")
            text [29024-29029] ("Тесты")
            newLine [29029-29030]
          section [29030-29389]
            text [29030-29159] ("Тесты пишутся весьма просто. От части потому что не нужно мокать кучу зависимостей. Функция в большинстве случаев самодостаточна.")
            newLine [29159-29160]
            newLine [29160-29161]
            srcBlock [29161-29252]
              blockHeader [29161-29183]
                keyword [29161-29183]
                  text [29161-29172] ("#+begin_src")
                  text [29172-29183] (" emacs-lisp")
              newLine [29183-29184]
              blockBody [29184-29242]
                text [29184-29242] ("(ert-deftest my-first-test ()\\n  (should (= (+ 10 10) 20)))")
              newLine [29242-29243]
              blockFooter [29243-29252]
                keyword [29243-29252]
                  text [29243-29252] ("#+end_src")
            newLine [29252-29253]
            text [29253-29260] ("Запуск.")
            newLine [29260-29261]
            newLine [29261-29262]
            srcBlock [29262-29365]
              blockHeader [29262-29278]
                keyword [29262-29278]
                  text [29262-29273] ("#+begin_src")
                  text [29273-29278] (" bash")
              newLine [29278-29279]
              blockBody [29279-29355]
                text [29279-29355] ("emacs -batch -l ert -l package.el -l test.el -f ert-run-tests-batch-and-exit")
              newLine [29355-29356]
              blockFooter [29356-29365]
                keyword [29356-29365]
                  text [29356-29365] ("#+end_src")
            newLine [29365-29366]
            undefined [29366-29389]
              blockHeader [29366-29382]
                keyword [29366-29382]
                  text [29366-29382] ("#+BEGIN_{HIDDEN}")
              newLine [29382-29383]
              blockBody [29383-29383]
              blockFooter [29383-29389]
                keyword [29383-29389]
                  text [29383-29389] ("#+END_")
        headline [29389-29485]
            :level 1:
          title [29383-29410]
            operator [29383-29385] ("* ")
            text [29385-29409] ("Статический анализ типов")
            newLine [29409-29410]
          section [29410-29479]
            link [29410-29478]
                :linkType network:
              operator [29410-29411] ("[")
              linkUrl [29411-29447]
                operator [29411-29412] ("[")
                text [29412-29446] ("https://github.com/emacs-elsa/Elsa")
                operator [29446-29447] ("]")
              linkName [29447-29477]
                operator [29447-29448] ("[")
                text [29448-29476] ("Его нет. Зато есть аннотации")
                operator [29476-29477] ("]")
              operator [29477-29478] ("]")
            newLine [29478-29479]
        headline [29485-31857]
            :level 1:
          title [29479-29501]
            operator [29479-29481] ("* ")
            text [29481-29490] ("Временно ")
            tagList [29490-29500]
              operator [29490-29491] (":")
              text [29491-29499] ("noexport")
              operator [29499-29500] (":")
            newLine [29500-29501]
          section [29501-31851]
            srcBlock [29501-29549]
              blockHeader [29501-29523]
                keyword [29501-29523]
                  text [29501-29512] ("#+begin_src")
                  text [29512-29523] (" emacs-lisp")
              newLine [29523-29524]
              blockBody [29524-29539]
                text [29524-29539] ("(message \\"\\"\\\\\\"\\")")
              newLine [29539-29540]
              blockFooter [29540-29549]
                keyword [29540-29549]
                  text [29540-29549] ("#+end_src")
            newLine [29549-29550]
            srcBlock [29550-29685]
              blockHeader [29550-29572]
                keyword [29550-29572]
                  text [29550-29561] ("#+begin_src")
                  text [29561-29572] (" emacs-lisp")
              newLine [29572-29573]
              blockBody [29573-29675]
                text [29573-29675] ("(message \\"%s\\" (string-match \\"{\\\\|);?$\\" \\"public replaceNonPrintableCharacters(text: string): string {\\"))")
              newLine [29679-29680]
              blockFooter [29680-29689]
                keyword [29680-29689]
                  text [29676-29685] ("#+end_src")
            newLine [29685-29686]
            keyword [29686-29697]
              text [29686-29696] ("#+RESULTS:")
              text [29696-29697] (" ")
            newLine [29697-29698]
            fixedWidth [29698-29702]
              operator [29698-29700] (": ")
              text [29700-29702] ("59")
            newLine [29702-29703]
            newLine [29703-29704]
            newLine [29704-29705]
            srcBlock [29705-29886]
              blockHeader [29705-29727]
                keyword [29705-29727]
                  text [29705-29716] ("#+begin_src")
                  text [29716-29727] (" emacs-lisp")
              newLine [29727-29728]
              blockBody [29728-29876]
                text [29728-29876] ("(setq turbo-log--ecmascript-final-symbols '(?; ?)))\\n(while (or (not (eobp)) (member ?) '(?; ?))))\\n                 (setq current-char char-after))))")
              newLine [29876-29877]
              blockFooter [29877-29886]
                keyword [29877-29886]
                  text [29877-29886] ("#+end_src")
            newLine [29886-29887]
            srcBlock [29887-30898]
              blockHeader [29887-29909]
                keyword [29887-29909]
                  text [29887-29898] ("#+begin_src")
                  text [29898-29909] (" emacs-lisp")
              newLine [29909-29910]
              blockBody [29910-30888]
                text [29910-30888] ("(setq quicktype-mode-configs '((\\"go\\" go-mode \\"\\")\\n                               (\\"ts\\" typescript-mode \\"\\")\\n                               (\\"js\\" js2-mode \\"\\")\\n                               (\\"rs\\" rust-mode \\"\\")\\n                               (\\"c++\\" c++-mode \\"\\")\\n                               (\\"javascript-prop-types\\" js2-mode \\"\\")\\n                               (\\"flow\\" flow-js2-mode \\"\\")\\n                               (\\"swift\\" swift-mode \\"\\")\\n                               (\\"kotlin\\" kotlin-mode \\"\\")\\n                               (\\"elm\\" elm-mode \\"\\")\\n                               (\\"ruby\\" ruby-mode \\"\\")\\n                               (\\"dart\\" dart-mode \\"\\")\\n                               (\\"py\\" python-mode \\"--python-version 3.7\\")\\n                               (\\"haskell\\" haskell-mode \\"\\")))\\n\\n;; (message \\"%s\\" quicktype-mode-configs)\\n(message \\"%s\\" (cl-rassoc 'go-mode quicktype-mode-configs :test #'member))\\n;; (message \\"%s\\" (cl-rassoc \\"Red Pine\\" needles-per-cluster :test #'member))")
              newLine [30888-30889]
              blockFooter [30889-30898]
                keyword [30889-30898]
                  text [30889-30898] ("#+end_src")
            newLine [30898-30899]
            keyword [30899-30910]
              text [30899-30909] ("#+RESULTS:")
              text [30909-30910] (" ")
            newLine [30910-30911]
            fixedWidth [30911-30926]
              operator [30911-30913] (": ")
              text [30913-30926] ("(go go-mode )")
            newLine [30926-30927]
            newLine [30927-30928]
            newLine [30928-30929]
            srcBlock [30929-31152]
              blockHeader [30929-30951]
                keyword [30929-30951]
                  text [30929-30940] ("#+begin_src")
                  text [30940-30951] (" emacs-lisp")
              newLine [30951-30952]
              blockBody [30952-31142]
                text [30952-31142] ("(setq needles-per-cluster\\n      '((2 \\"Austrian Pine\\" \\"Red Pine\\")\\n        (3 \\"Pitch Pine\\")\\n        (5 \\"White Pine\\")))\\n\\n(message \\"%s\\" (cl-rassoc \\"Red Pine\\" needles-per-cluster :test #'member))")
              newLine [31142-31143]
              blockFooter [31143-31152]
                keyword [31143-31152]
                  text [31143-31152] ("#+end_src")
            newLine [31152-31153]
            keyword [31153-31164]
              text [31153-31163] ("#+RESULTS:")
              text [31163-31164] (" ")
            newLine [31164-31165]
            fixedWidth [31165-31193]
              operator [31165-31167] (": ")
              text [31167-31193] ("(2 Austrian Pine Red Pine)")
            newLine [31193-31194]
            newLine [31194-31195]
            newLine [31195-31196]
            srcBlock [31196-31332]
              blockHeader [31196-31218]
                keyword [31196-31218]
                  text [31196-31207] ("#+begin_src")
                  text [31207-31218] (" emacs-lisp")
              newLine [31218-31219]
              blockBody [31219-31322]
                text [31219-31322] ("(message \\"%s\\" (string-match \\"\\\\({\\\\|;$\\\\)\\\\|\\\\(const [\\\\w\\\\[:digit]]+ = [\\\\d[:digit:]]+$\\\\)\\" \\"  const foo = 1\\"))")
              newLine [31322-31323]
              blockFooter [31323-31332]
                keyword [31323-31332]
                  text [31323-31332] ("#+end_src")
            newLine [31332-31333]
            keyword [31333-31344]
              text [31333-31343] ("#+RESULTS:")
              text [31343-31344] (" ")
            newLine [31344-31345]
            fixedWidth [31345-31350]
              operator [31345-31347] (": ")
              text [31347-31350] ("nil")
            newLine [31350-31351]
            text [31351-31359] ("{HIDDEN}")
            newLine [31359-31360]
            newLine [31360-31361]
            srcBlock [31361-31461]
              blockHeader [31361-31383]
                keyword [31361-31383]
                  text [31361-31372] ("#+begin_src")
                  text [31372-31383] (" emacs-lisp")
              newLine [31383-31384]
              blockBody [31384-31451]
                text [31384-31451] ("(setq v (dolist (i '(1 2 3 4))\\n                i))\\n(message \\"%s\\" v)")
              newLine [31451-31452]
              blockFooter [31452-31461]
                keyword [31452-31461]
                  text [31452-31461] ("#+end_src")
            newLine [31461-31462]
            keyword [31462-31473]
              text [31462-31472] ("#+RESULTS:")
              text [31472-31473] (" ")
            newLine [31473-31474]
            fixedWidth [31474-31479]
              operator [31474-31476] (": ")
              text [31476-31479] ("nil")
            newLine [31479-31480]
            newLine [31480-31481]
            newLine [31481-31482]
            headline [31482-31851]
                :level 2:
              title [31482-31496]
                operator [31482-31485] ("** ")
                text [31485-31495] ("Check json")
                newLine [31495-31496]
              section [31496-31851]
                srcBlock [31496-31840]
                  blockHeader [31496-31518]
                    keyword [31496-31518]
                      text [31496-31507] ("#+begin_src")
                      text [31507-31518] (" emacs-lisp")
                  newLine [31518-31519]
                  blockBody [31519-31830]
                    text [31519-31830] ("  (let* ((json-object-type 'plist)\\n         (json-array-type 'list)\\n         (json-key-type 'string)\\n         (json (json-read-file web-roam-configuration-file-path))\\n         (name-to-config (make-hash-table :test 'equal))\\n         (server-names '()))\\n    (dolist (config json)\\n      (message \\"%s\\" config))\\n  )")
                  newLine [31830-31831]
                  blockFooter [31831-31840]
                    keyword [31831-31840]
                      text [31831-31840] ("#+end_src")
                newLine [31840-31841]
                keyword [31841-31851]
                  text [31841-31851] ("#+RESULTS:")
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
      "root [0-2188]
          :id  scss:
        propertyDrawer [0-28]
          property [0-12]
            text [0-12] (":PROPERTIES:")
          newLine [12-13]
          property [13-22]
            text [13-17] (":ID:")
            text [17-22] (" scss")
          newLine [22-23]
          property [23-28]
            text [23-28] (":END:")
        newLine [28-29]
        newLine [29-30]
        keyword [30-47]
          text [30-38] ("#+TITLE:")
          text [38-47] (" Верстка.")
        newLine [47-48]
        keyword [48-92]
          text [48-62] ("#+DESCRIPTION:")
          text [62-92] (" Подборка всякого для верстки.")
        newLine [92-93]
        keyword [93-127]
          text [93-104] ("#+FILETAGS:")
          text [104-105] (" ")
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
          text [128-133] ("#+ID:")
          text [133-138] (" scss")
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
                  blockHeader [989-1005]
                    keyword [989-1005]
                      text [989-1000] ("#+BEGIN_SRC")
                      text [1000-1005] (" scss")
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
                    text [1097-1101] (":ID:")
                    text [1101-1120] (" css-hide-scrollbar")
                  newLine [1120-1121]
                  property [1121-1126]
                    text [1121-1126] (":END:")
                newLine [1126-1127]
                srcBlock [1127-1201]
                  blockHeader [1127-1142]
                    keyword [1127-1142]
                      text [1127-1138] ("#+BEGIN_SRC")
                      text [1138-1142] (" css")
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
                  text [1248-1259] ("#+BEGIN_SRC")
                  text [1259-1264] (" scss")
                newLine [1264-1265]
                text [1265-1378] (".example {\\n  overflow: hidden;\\n  display: -webkit-box;\\n  -webkit-line-clamp: 3;\\n  -webkit-box-orient: vertical;\\n}")
                newLine [1378-1379]
                newLine [1379-1380]
        headline [1380-2188]
            :level 1:
          title [1380-1390]
            operator [1380-1382] ("* ")
            text [1382-1389] ("Миксины")
            newLine [1389-1390]
          section [1390-2188]
            headline [1390-2188]
                :level 2:
              title [1390-1417]
                operator [1390-1393] ("** ")
                text [1393-1416] ("Mixin для media queries")
                newLine [1416-1417]
              section [1417-2188]
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
                srcBlock [1491-2188]
                  blockHeader [1491-1507]
                    keyword [1491-1507]
                      text [1491-1502] ("#+BEGIN_SRC")
                      text [1502-1507] (" scss")
                  newLine [1507-1508]
                  blockBody [1508-2178]
                    text [1508-2178] ("@mixin breakpoint($breakpoint, $direction) {\\n  @if map-has-key($breakpoints, $breakpoint) {\\n\\n    // Get the breakpoint value.\\n    $breakpoint-value: map-get($breakpoints, $breakpoint);\\n\\n    @if $direction == max {{\\n      @media (max-width: ($breakpoint-value - 1)) {{\\n        @content;\\n      \\n     @else if $direction == min {{\\n      @media (min-width: $breakpoint-value) {{\\n        @content;\\n      \\n    \\n\\n  // If the breakpoint doesn't exist in the map.\\n   @else {\\n    @if $direction == max {{\\n      @media (max-width: $breakpoint) {{\\n        @content;\\n      \\n     @else if $direction == min {{\\n      @media (min-width: $breakpoint) {\\n        @content;\\n      \\n    \\n  \\n\\n")
                  newLine [2179-2180]
                  blockFooter [2180-2189]
                    keyword [2180-2189]
                      text [2179-2188] ("#+END_SRC")
      "
    `);
  });
});
