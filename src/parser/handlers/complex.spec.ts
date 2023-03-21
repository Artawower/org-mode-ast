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
      "root [0-32125]
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
        headline [308-28848]
            :level 1:
          title [308-361]
            operator [308-310] ("* ")
            text [310-360] ("Elisp - расширеяемый функциональный язык для emacs")
            newLine [360-361]
          section [361-28848]
            text [361-537] ("Все что я опишу ниже - плод моего изучения. Рекомендую изучать язык по ссылкам приведенным ниже. Я могу ошибаться, а также неправильно интерпритировать изученный мной материал.")
            newLine [537-538]
            text [538-784] ("Также, может показаться что я отношусь к лиспу как к неочень хорошо спроектированному языку. Это не так. Я отношусь так ко всем языкам. При этом автор понятия не имеет как можно что-то улучшить, и вообще... не стоит тратить время на его писульки.")
            newLine [784-785]
            newLine [785-786]
            headline [786-2285]
                :level 2:
              title [786-796]
                operator [786-789] ("** ")
                text [789-795] ("Ссылки")
                newLine [795-796]
              section [796-2285]
                keyword [796-839]
                  text [796-839] ("#+START_{SPOILER} Ресурсы для ознакомления ")
                text [839-840] (">")
                newLine [840-841]
                newLine [841-842]
                list [842-2164]
                    :unordered:
                    :level 0:
                  listItem [842-928]
                    title [842-928]
                      operator [842-844] ("+ ")
                      link [844-927]
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
                  listItem [1989-2164]
                    title [1989-2164]
                      operator [1989-1991] ("+ ")
                      link [1991-2091]
                        operator [1991-1992] ("[")
                        linkUrl [1992-2038]
                          operator [1992-1993] ("[")
                          text [1993-2037] ("htest-varps://gitlab.com/niklaseklund/dtache")
                          operator [2037-2038] ("]")
                        linkName [2038-2090]
                          operator [2038-2039] ("[")
                          text [2039-2089] ("Dtache - пакет для запуска изолированных процессов")
                          operator [2089-2090] ("]")
                        operator [2090-2091] ("]")
                      text [2091-2093] (" (")
                      link [2093-2162]
                        operator [2093-2094] ("[")
                        linkUrl [2094-2139]
                          operator [2094-2095] ("[")
                          text [2095-2138] ("https://www.youtube.com/watch?v=if1W58SrClk")
                          operator [2138-2139] ("]")
                        linkName [2139-2161]
                          operator [2139-2140] ("[")
                          text [2140-2160] ("тут можно посмотреть")
                          operator [2160-2161] ("]")
                        operator [2161-2162] ("]")
                      text [2162-2163] (")")
                      newLine [2163-2164]
                keyword [2164-2181]
                  text [2164-2181] ("#+CLOSE_{SPOILER}")
                newLine [2181-2182]
                headline [2182-2216]
                    :level 2:
                  title [2007-2041]
                    operator [2007-2010] ("** ")
                    text [2010-2040] ("Пакеты в помощь для разработки")
                    newLine [2040-2041]
                  section [2041-2041]
                headline [2216-2231]
                    :level 1:
                  title [2216-2231]
                    operator [2216-2218] ("* ")
                    text [2218-2230] ("Quick Start.")
                    newLine [2230-2231]
                  section [2231-2231]
                text [2231-2283] ("Быстрый старт для тех кто уже умеет программировать.")
                newLine [2283-2284]
                newLine [2284-2285]
            headline [2285-10793]
                :level 2:
              title [2285-2323]
                operator [2285-2288] ("** ")
                text [2288-2322] ("Типы данных, переменные, константы")
                newLine [2322-2323]
              section [2323-10793]
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
                headline [5293-7774]
                    :level 3:
                  title [5293-5304]
                    operator [5293-5297] ("*** ")
                    text [5297-5303] ("Списки")
                    newLine [5303-5304]
                  section [5304-7774]
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
                    headline [7069-7519]
                        :level 4:
                      title [7069-7115]
                        operator [7069-7074] ("**** ")
                        text [7074-7114] ("Перезаписать элемент в списке по индексу")
                        newLine [7114-7115]
                      section [7115-7519]
                        srcBlock [7115-7272]
                          blockHeader [7115-7137]
                            keyword [7115-7137]
                              text [7115-7126] ("#+begin_src")
                              text [7126-7137] (" emacs-lisp")
                          newLine [7137-7138]
                          blockBody [7138-7262]
                            text [7138-7262] ("(setq my-test-list '((\\"qwe\\" . 1) (\\"be\\" . 2)))\\n(setcdr (assoc \\"qwe\\" my-test-list) \\"asdlkajsdakd\\")\\n(message \\"%s\\" my-test-list)")
                          newLine [7262-7263]
                          blockFooter [7263-7272]
                            keyword [7263-7272]
                              text [7263-7272] ("#+end_src")
                        newLine [7272-7273]
                        keyword [7273-7284]
                          text [7273-7283] ("#+RESULTS:")
                          text [7283-7284] (" ")
                        newLine [7284-7285]
                        fixedWidth [7285-7318]
                          operator [7285-7287] (": ")
                          text [7287-7318] ("((qwe . asdlkajsdakd) (be . 2))")
                        newLine [7318-7319]
                        newLine [7319-7320]
                        text [7320-7359] ("А что если этого элемента нет в списке?")
                        newLine [7359-7360]
                        srcBlock [7360-7505]
                          blockHeader [7360-7382]
                            keyword [7360-7382]
                              text [7360-7371] ("#+begin_src")
                              text [7371-7382] (" emacs-lisp")
                          newLine [7382-7383]
                          blockBody [7383-7495]
                            text [7383-7495] ("(setq my-test-list '((\\"be\\" . 2)))\\n(setcdr (assoc \\"qwe\\" my-test-list) \\"asdlkajsdakd\\")\\n(message \\"%s\\" my-test-list)")
                          newLine [7495-7496]
                          blockFooter [7496-7505]
                            keyword [7496-7505]
                              text [7496-7505] ("#+end_src")
                        newLine [7505-7506]
                        newLine [7506-7507]
                        text [7507-7518] ("Не работает")
                        newLine [7518-7519]
                    headline [7519-7774]
                        :level 4:
                      title [7519-7550]
                        operator [7519-7524] ("**** ")
                        text [7524-7549] ("Удалить элемент из списка")
                        newLine [7549-7550]
                      section [7550-7774]
                        srcBlock [7550-7771]
                          blockHeader [7550-7588]
                            keyword [7550-7573]
                              text [7550-7561] ("#+BEGIN_SRC")
                              text [7561-7573] (" emacs-lisp ")
                            blockProperty [7573-7588]
                              text [7573-7581] (":results")
                              text [7581-7588] (" silent")
                          newLine [7588-7589]
                          blockBody [7589-7761]
                            text [7589-7761] ("ELISP> (setq list1 '(alpha beta gamma))\\n (alpha beta gamma)\\n \\n ELISP> (setq list2 (delete 'beta list1))\\n (alpha gamma)\\n \\n ELISP> (setq list3 (delete 'alpha list1))\\n (gamma)")
                          newLine [7761-7762]
                          blockFooter [7762-7771]
                            keyword [7762-7771]
                              text [7762-7771] ("#+END_SRC")
                        newLine [7771-7772]
                        newLine [7772-7773]
                        newLine [7773-7774]
                headline [7774-9987]
                    :level 3:
                  title [7774-7800]
                    operator [7774-7778] ("*** ")
                    text [7778-7799] ("Ассоциативные массивы")
                    newLine [7799-7800]
                  section [7800-9987]
                    headline [7800-8128]
                        :level 4:
                      title [7800-7816]
                        operator [7800-7805] ("**** ")
                        text [7805-7815] ("Объявление")
                        newLine [7815-7816]
                      section [7816-8128]
                        srcBlock [7816-7884]
                          blockHeader [7816-7838]
                            keyword [7816-7838]
                              text [7816-7827] ("#+begin_src")
                              text [7827-7838] (" emacs-lisp")
                          newLine [7838-7839]
                          blockBody [7839-7874]
                            text [7839-7874] ("(setq trees '((a . 1) (b . \\"qwe\\")))")
                          newLine [7874-7875]
                          blockFooter [7875-7884]
                            keyword [7875-7884]
                              text [7875-7884] ("#+end_src")
                        newLine [7884-7885]
                        keyword [7885-7896]
                          text [7885-7895] ("#+RESULTS:")
                          text [7895-7896] (" ")
                        newLine [7896-7897]
                        fixedWidth [7897-7918]
                          operator [7897-7899] (": ")
                          text [7899-7918] ("((a . 1) (b . qwe))")
                        newLine [7918-7919]
                        newLine [7919-7920]
                        text [7920-7962] ("При чем точка нужна для специального типа ")
                        italic [7962-7971]
                          operator [7962-7963] ("/")
                          text [7963-7970] ("symbols")
                          operator [7970-7971] ("/")
                        text [7971-8028] (". Если работает с реальными значениями то можно и без нее")
                        newLine [8028-8029]
                        newLine [8029-8030]
                        srcBlock [8030-8127]
                          blockHeader [8030-8052]
                            keyword [8030-8052]
                              text [8030-8041] ("#+begin_src")
                              text [8041-8052] (" emacs-lisp")
                          newLine [8052-8053]
                          blockBody [8053-8117]
                            text [8053-8117] ("(setq another-hashmap '((\\"a\\" \\"First elem\\") (\\"b\\" \\"Second elem\\")))")
                          newLine [8117-8118]
                          blockFooter [8118-8127]
                            keyword [8118-8127]
                              text [8118-8127] ("#+end_src")
                        newLine [8127-8128]
                    headline [8128-8426]
                        :level 4:
                      title [8128-8159]
                        operator [8128-8133] ("**** ")
                        text [8133-8158] ("Получить элемент по ключу")
                        newLine [8158-8159]
                      section [8159-8426]
                        srcBlock [8159-8223]
                          blockHeader [8159-8181]
                            keyword [8159-8181]
                              text [8159-8170] ("#+begin_src")
                              text [8170-8181] (" emacs-lisp")
                          newLine [8181-8182]
                          blockBody [8182-8213]
                            text [8182-8213] ("(message \\"%s\\" (assoc 'a trees))")
                          newLine [8213-8214]
                          blockFooter [8214-8223]
                            keyword [8214-8223]
                              text [8214-8223] ("#+end_src")
                        newLine [8223-8224]
                        text [8224-8332] ("Ну и конечно возвращает оно кортеж..а чтобы получить элемент нужно использовать уже известную нам функцию - ")
                        verbatim [8332-8337]
                          operator [8332-8333] ("=")
                          text [8333-8336] ("cdr")
                          operator [8336-8337] ("=")
                        newLine [8337-8338]
                        newLine [8338-8339]
                        srcBlock [8339-8409]
                          blockHeader [8339-8361]
                            keyword [8339-8361]
                              text [8339-8350] ("#+begin_src")
                              text [8350-8361] (" emacs-lisp")
                          newLine [8361-8362]
                          blockBody [8362-8399]
                            text [8362-8399] ("(message \\"%s\\" (cdr (assoc 'a trees)))")
                          newLine [8399-8400]
                          blockFooter [8400-8409]
                            keyword [8400-8409]
                              text [8400-8409] ("#+end_src")
                        newLine [8409-8410]
                        keyword [8410-8421]
                          text [8410-8420] ("#+RESULTS:")
                          text [8420-8421] (" ")
                        newLine [8421-8422]
                        fixedWidth [8422-8425]
                          operator [8422-8424] (": ")
                          text [8424-8425] ("1")
                        newLine [8425-8426]
                    headline [8426-8752]
                        :level 4:
                      title [8426-8460]
                        operator [8426-8431] ("**** ")
                        text [8431-8459] ("Получить элемент по значению")
                        newLine [8459-8460]
                      section [8460-8752]
                        srcBlock [8460-8528]
                          blockHeader [8460-8482]
                            keyword [8460-8482]
                              text [8460-8471] ("#+begin_src")
                              text [8471-8482] (" emacs-lisp")
                          newLine [8482-8483]
                          blockBody [8483-8518]
                            text [8483-8518] ("(message \\"%s\\" (rassoc \\"qwe\\" trees))")
                          newLine [8518-8519]
                          blockFooter [8519-8528]
                            keyword [8519-8528]
                              text [8519-8528] ("#+end_src")
                        newLine [8528-8529]
                        text [8529-8538] ("При этом ")
                        italic [8538-8546]
                          operator [8538-8539] ("/")
                          text [8539-8545] ("rassoc")
                          operator [8545-8546] ("/")
                        text [8546-8587] (" работает и для строк и для чисел, а вот ")
                        italic [8587-8594]
                          operator [8587-8588] ("/")
                          text [8588-8593] ("rassq")
                          operator [8593-8594] ("/")
                        text [8594-8611] (" только для чисел")
                        newLine [8611-8612]
                        newLine [8612-8613]
                        srcBlock [8613-8729]
                          blockHeader [8613-8635]
                            keyword [8613-8635]
                              text [8613-8624] ("#+begin_src")
                              text [8624-8635] (" emacs-lisp")
                          newLine [8635-8636]
                          blockBody [8636-8719]
                            text [8636-8719] ("(message \\"%s\\" (rassq \\"qwe\\" trees)) ;; nil\\n(message \\"%s\\" (rassq 1 trees)) ;; (a . 1)")
                          newLine [8719-8720]
                          blockFooter [8720-8729]
                            keyword [8720-8729]
                              text [8720-8729] ("#+end_src")
                        newLine [8729-8730]
                        keyword [8730-8741]
                          text [8730-8740] ("#+RESULTS:")
                          text [8740-8741] (" ")
                        newLine [8741-8742]
                        fixedWidth [8742-8751]
                          operator [8742-8744] (": ")
                          text [8744-8751] ("(a . 1)")
                        newLine [8751-8752]
                    headline [8752-9086]
                        :level 4:
                      title [8752-8774]
                        operator [8752-8757] ("**** ")
                        text [8757-8773] ("Копирование мапы")
                        newLine [8773-8774]
                      section [8774-9086]
                        srcBlock [8774-9012]
                          blockHeader [8774-8796]
                            keyword [8774-8796]
                              text [8774-8785] ("#+begin_src")
                              text [8785-8796] (" emacs-lisp")
                          newLine [8796-8797]
                          blockBody [8797-9002]
                            text [8797-9002] ("  (setq needles-per-cluster\\n        '((2 . (\\"Austrian Pine\\" \\"Red Pine\\"))\\n          (3 . (\\"Pitch Pine\\"))\\n          (5 . (\\"White Pine\\"))))\\n  (setq copy (copy-alist needles-per-cluster))\\n  (message \\"%s\\" copy)")
                          newLine [9002-9003]
                          blockFooter [9003-9012]
                            keyword [9003-9012]
                              text [9003-9012] ("#+end_src")
                        newLine [9012-9013]
                        keyword [9013-9024]
                          text [9013-9023] ("#+RESULTS:")
                          text [9023-9024] (" ")
                        newLine [9024-9025]
                        fixedWidth [9025-9085]
                          operator [9025-9027] (": ")
                          text [9027-9085] ("((2 Austrian Pine Red Pine) (3 Pitch Pine) (5 White Pine))")
                        newLine [9085-9086]
                    headline [9086-9490]
                        :level 4:
                      title [9086-9122]
                        operator [9086-9091] ("**** ")
                        text [9091-9121] ("Удаление всех записей по ключу")
                        newLine [9121-9122]
                      section [9122-9490]
                        srcBlock [9122-9414]
                          blockHeader [9122-9144]
                            keyword [9122-9144]
                              text [9122-9133] ("#+begin_src")
                              text [9133-9144] (" emacs-lisp")
                          newLine [9144-9145]
                          blockBody [9145-9404]
                            text [9145-9404] ("  (setq alist (list '(foo 1) '(bar 2) '(foo 3) '(lose 4)))\\n  (setq new-alist (assq-delete-all 'foo alist)) ;; Возвращает новое значение\\n  (message \\"%s\\" new-alist)\\n  (message (concat (format \\"alist: %s\\n\\" alist)\\n                   (format \\"new: %s\\" new-alist)))")
                          newLine [9404-9405]
                          blockFooter [9405-9414]
                            keyword [9405-9414]
                              text [9405-9414] ("#+end_src")
                        newLine [9414-9415]
                        keyword [9415-9426]
                          text [9415-9425] ("#+RESULTS:")
                          text [9425-9426] (" ")
                        newLine [9426-9427]
                        fixedWidth [9427-9462]
                          operator [9427-9429] (": ")
                          text [9429-9462] ("alist: ((foo 1) (bar 2) (lose 4))")
                        newLine [9462-9463]
                        fixedWidth [9463-9489]
                          operator [9463-9465] (": ")
                          text [9465-9489] (" new: ((bar 2) (lose 4))")
                        newLine [9489-9490]
                    headline [9490-9987]
                        :level 4:
                      title [9490-9524]
                        operator [9490-9495] ("**** ")
                        text [9495-9523] ("Удаление записей по значению")
                        newLine [9523-9524]
                      section [9524-9987]
                        srcBlock [9524-9870]
                          blockHeader [9524-9546]
                            keyword [9524-9546]
                              text [9524-9535] ("#+begin_src")
                              text [9535-9546] (" emacs-lisp")
                          newLine [9546-9547]
                          blockBody [9547-9860]
                            text [9547-9860] ("  (setq alist2 '((foo . first) (bar . second) (foo2 . third) (qwe . five)))\\n  (setq new-alist (rassq-delete-all 'third alist2)) ;; меняет значение ?\\n  (message \\"%s\\" new-alist)\\n  (message (concat (format \\"alist: %s\\n\\" alist2)\\n                   (format \\"new: %s\\" new-alist)))\\n  ;; (message \\"%s\\" (rassq 'foo alist2))")
                          newLine [9860-9861]
                          blockFooter [9861-9870]
                            keyword [9861-9870]
                              text [9861-9870] ("#+end_src")
                        newLine [9870-9871]
                        keyword [9871-9882]
                          text [9871-9881] ("#+RESULTS:")
                          text [9881-9882] (" ")
                        newLine [9882-9883]
                        fixedWidth [9883-9935]
                          operator [9883-9885] (": ")
                          text [9885-9935] ("alist: ((foo . first) (bar . second) (qwe . five))")
                        newLine [9935-9936]
                        fixedWidth [9936-9986]
                          operator [9936-9938] (": ")
                          text [9938-9986] ("new: ((foo . first) (bar . second) (qwe . five))")
                        newLine [9986-9987]
                headline [9987-10655]
                    :level 3:
                  title [9987-9998]
                    operator [9987-9991] ("*** ")
                    text [9991-9997] ("Хешмап")
                    newLine [9997-9998]
                  section [9998-10655]
                    link [9998-10070]
                      operator [9998-9999] ("[")
                      linkUrl [9999-10055]
                        operator [9999-10000] ("[")
                        text [10000-10054] ("htest-varp://ergoemacs.org/emacs/elisp_hash_table.html")
                        operator [10054-10055] ("]")
                      linkName [10055-10069]
                        operator [10055-10056] ("[")
                        text [10056-10068] ("Документация")
                        operator [10068-10069] ("]")
                      operator [10069-10070] ("]")
                    newLine [10070-10071]
                    newLine [10071-10072]
                    srcBlock [10072-10626]
                      blockHeader [10072-10094]
                        keyword [10072-10094]
                          text [10072-10083] ("#+begin_src")
                          text [10083-10094] (" emacs-lisp")
                      newLine [10094-10095]
                      blockBody [10095-10616]
                        text [10095-10616] ("  (setq my-first-map #s(\\n                        hash-table\\n                        size 10\\n                        test equal\\n                        data (\\n                              python-mode \\"spam!\\"\\n                              go-mode \\"booo!1 terrible pointer\\"\\n                              org-mode \\"amma fluffy feature ;p\\"\\n                              )))\\n  (puthash 'js-mode \\"ugly language\\" my-first-map)\\n  (message \\"%s\\" (gethash 'python-mode my-first-map))\\n  (message \\"%s\\" (gethash 'js-mode my-first-map))")
                      newLine [10616-10617]
                      blockFooter [10617-10626]
                        keyword [10617-10626]
                          text [10617-10626] ("#+end_src")
                    newLine [10626-10627]
                    keyword [10627-10638]
                      text [10627-10637] ("#+RESULTS:")
                      text [10637-10638] (" ")
                    newLine [10638-10639]
                    fixedWidth [10639-10654]
                      operator [10639-10641] (": ")
                      text [10641-10654] ("ugly language")
                    newLine [10654-10655]
                headline [10655-10793]
                    :level 3:
                  title [10655-10666]
                    operator [10655-10659] ("*** ")
                    text [10659-10665] ("Символ")
                    newLine [10665-10666]
                  section [10666-10793]
                    text [10666-10759] ("Тип данные соотвутствующий объекту с именем. Задаются символы с помощью 1 начальной кавычки. ")
                    verbatim [10759-10773]
                      operator [10759-10760] ("=")
                      text [10760-10772] ("'amma-symbol")
                      operator [10772-10773] ("=")
                    newLine [10773-10774]
                    newLine [10774-10775]
                    keyword [10775-10792]
                      text [10775-10792] ("#+CLOSE_{SPOILER}")
                    newLine [10792-10793]
            headline [10793-14274]
                :level 2:
              title [10793-10804]
                operator [10793-10796] ("** ")
                text [10796-10803] ("Функции")
                newLine [10803-10804]
              section [10804-14274]
                keyword [10804-10841]
                  text [10804-10841] ("#+START_{SPOILER} Читать про функции ")
                text [10841-10842] (">")
                newLine [10842-10843]
                newLine [10843-10844]
                headline [10844-11305]
                    :level 3:
                  title [10844-10867]
                    operator [10844-10848] ("*** ")
                    text [10848-10866] ("Объявление функций")
                    newLine [10866-10867]
                  section [10867-11305]
                    text [10867-10952] ("Функции принято комментировать, это позволяет смотреть документацию в автодополнении.")
                    newLine [10952-10953]
                    text [10953-10959] ("Вызов ")
                    verbatim [10959-10974]
                      operator [10959-10960] ("=")
                      text [10960-10973] ("(interactive)")
                      operator [10973-10974] ("=")
                    text [10974-11081] (" означается что функция публичная и может быть взывана пользователем напрямую, либо через сочетание клавиш.")
                    newLine [11081-11082]
                    newLine [11082-11083]
                    srcBlock [11083-11272]
                      blockHeader [11083-11105]
                        keyword [11083-11105]
                          text [11083-11094] ("#+begin_src")
                          text [11094-11105] (" emacs-lisp")
                      newLine [11105-11106]
                      blockBody [11106-11262]
                        text [11106-11262] ("  (defun hello (my-name)\\n    \\"This function will say hello for MY-NAME.\\"\\n    (interactive)\\n    (message (concat \\"Hello, I'am \\" my-name)))\\n\\n  (hello \\"Artur\\")")
                      newLine [11262-11263]
                      blockFooter [11263-11272]
                        keyword [11263-11272]
                          text [11263-11272] ("#+end_src")
                    newLine [11272-11273]
                    keyword [11273-11284]
                      text [11273-11283] ("#+RESULTS:")
                      text [11283-11284] (" ")
                    newLine [11284-11285]
                    fixedWidth [11285-11304]
                      operator [11285-11287] (": ")
                      text [11287-11304] ("Hello, I’am Artur")
                    newLine [11304-11305]
                headline [11305-11586]
                    :level 3:
                  title [11305-11332]
                    operator [11305-11309] ("*** ")
                    text [11309-11331] ("Опицональные аргументы")
                    newLine [11331-11332]
                  section [11332-11586]
                    srcBlock [11332-11555]
                      blockHeader [11332-11354]
                        keyword [11332-11354]
                          text [11332-11343] ("#+begin_src")
                          text [11343-11354] (" emacs-lisp")
                      newLine [11354-11355]
                      blockBody [11355-11545]
                        text [11355-11545] ("(defun my-super-optional-function (name &optional last-name patronymic)\\n  (message \\"%s %s %s\\" name (or last-name \\"\\") (or patronymic \\"\\")))\\n\\n(my-super-optional-function \\"Artur\\" nil \\"Proshkov\\")")
                      newLine [11545-11546]
                      blockFooter [11546-11555]
                        keyword [11546-11555]
                          text [11546-11555] ("#+end_src")
                    newLine [11555-11556]
                    keyword [11556-11567]
                      text [11556-11566] ("#+RESULTS:")
                      text [11566-11567] (" ")
                    newLine [11567-11568]
                    fixedWidth [11568-11585]
                      operator [11568-11570] (": ")
                      text [11570-11585] ("Artur  Proshkov")
                    newLine [11585-11586]
                headline [11586-11903]
                    :level 3:
                  title [11586-11612]
                    operator [11586-11590] ("*** ")
                    text [11590-11611] ("Именованные аргументы")
                    newLine [11611-11612]
                  section [11612-11903]
                    srcBlock [11612-11861]
                      blockHeader [11612-11634]
                        keyword [11612-11634]
                          text [11612-11623] ("#+begin_src")
                          text [11623-11634] (" emacs-lisp")
                      newLine [11634-11635]
                      blockBody [11635-11851]
                        text [11635-11851] ("(defun my-super-function-with-named-args (&rest args)\\n  (message \\"Name %s, middle name %s\\" (plist-get args :name) (plist-get args :middle-name)))\\n\\n  (my-super-function-with-named-args :name \\"One\\" :middle-name \\"Dude\\")")
                      newLine [11851-11852]
                      blockFooter [11852-11861]
                        keyword [11852-11861]
                          text [11852-11861] ("#+end_src")
                    newLine [11861-11862]
                    keyword [11862-11873]
                      text [11862-11872] ("#+RESULTS:")
                      text [11872-11873] (" ")
                    newLine [11873-11874]
                    fixedWidth [11874-11902]
                      operator [11874-11876] (": ")
                      text [11876-11902] ("Name One, middle name Dude")
                    newLine [11902-11903]
                headline [11903-12087]
                    :level 3:
                  title [11903-11914]
                    operator [11903-11907] ("*** ")
                    text [11907-11913] ("Лямбды")
                    newLine [11913-11914]
                  section [11914-12087]
                    crossed [11914-11971]
                      operator [11914-11915] ("+")
                      text [11915-11970] ("Очевидно, лямбды нужны чтобы код можно было хуже читать")
                      operator [11970-11971] ("+")
                    newLine [11971-11972]
                    newLine [11972-11973]
                    srcBlock [11973-12056]
                      blockHeader [11973-11995]
                        keyword [11973-11995]
                          text [11973-11984] ("#+begin_src")
                          text [11984-11995] (" emacs-lisp")
                      newLine [11995-11996]
                      blockBody [11996-12046]
                        text [11996-12046] ("(funcall '(lambda () (message \\"I'am dirty func\\")))")
                      newLine [12046-12047]
                      blockFooter [12047-12056]
                        keyword [12047-12056]
                          text [12047-12056] ("#+end_src")
                    newLine [12056-12057]
                    keyword [12057-12068]
                      text [12057-12067] ("#+RESULTS:")
                      text [12067-12068] (" ")
                    newLine [12068-12069]
                    fixedWidth [12069-12086]
                      operator [12069-12071] (": ")
                      text [12071-12086] ("I’am dirty func")
                    newLine [12086-12087]
                headline [12087-12664]
                    :level 3:
                  title [12087-12098]
                    operator [12087-12091] ("*** ")
                    text [12091-12097] ("Advice")
                    newLine [12097-12098]
                  section [12098-12664]
                    text [12098-12200] ("Адвайсы это прокаченные декораторы. Могут быть вызваны как до так и после вызова оригинальной функции.")
                    newLine [12200-12201]
                    newLine [12201-12202]
                    srcBlock [12202-12382]
                      blockHeader [12202-12224]
                        keyword [12202-12224]
                          text [12202-12213] ("#+begin_src")
                          text [12213-12224] (" emacs-lisp")
                      newLine [12224-12225]
                      blockBody [12225-12372]
                        text [12225-12372] ("(defun my-increment (n)\\n  (+ n 1))\\n\\n(defun mux-5 (n)\\n  (* n 5))\\n\\n(advice-add 'my-increment :filter-return #'mux-5)\\n(message \\"%s\\" (my-increment 10))")
                      newLine [12372-12373]
                      blockFooter [12373-12382]
                        keyword [12373-12382]
                          text [12373-12382] ("#+end_src")
                    newLine [12382-12383]
                    keyword [12383-12394]
                      text [12383-12393] ("#+RESULTS:")
                      text [12393-12394] (" ")
                    newLine [12394-12395]
                    fixedWidth [12395-12399]
                      operator [12395-12397] (": ")
                      text [12397-12399] ("55")
                    newLine [12399-12400]
                    bold [12400-12442]
                      operator [12400-12401] ("*")
                      text [12401-12441] ("Пример адвайса после выполненеия функции")
                      operator [12441-12442] ("*")
                    newLine [12442-12443]
                    newLine [12443-12444]
                    srcBlock [12444-12642]
                      blockHeader [12444-12466]
                        keyword [12444-12466]
                          text [12444-12455] ("#+begin_src")
                          text [12455-12466] (" emacs-lisp")
                      newLine [12466-12467]
                      blockBody [12467-12632]
                        text [12467-12632] ("(defun my-first-func()\\n  (message \\"qweqwe\\"))\\n(my-first-func)\\n(defun my-adv()\\n  (message \\"advice called\\"))\\n(advice-add :after 'my-first-func #'my-adv)\\n(my-first-func)")
                      newLine [12632-12633]
                      blockFooter [12633-12642]
                        keyword [12633-12642]
                          text [12633-12642] ("#+end_src")
                    newLine [12642-12643]
                    keyword [12643-12654]
                      text [12643-12653] ("#+RESULTS:")
                      text [12653-12654] (" ")
                    newLine [12654-12655]
                    fixedWidth [12655-12663]
                      operator [12655-12657] (": ")
                      text [12657-12663] ("qweqwe")
                    newLine [12663-12664]
                headline [12664-13871]
                    :level 3:
                  title [12664-12690]
                    operator [12664-12668] ("*** ")
                    text [12668-12689] ("Property list (plist)")
                    newLine [12689-12690]
                  section [12690-13871]
                    bold [12690-12710]
                      operator [12690-12691] ("*")
                      text [12691-12709] ("Установка и запись")
                      operator [12709-12710] ("*")
                    newLine [12710-12711]
                    newLine [12711-12712]
                    srcBlock [12712-12936]
                      blockHeader [12712-12734]
                        keyword [12712-12734]
                          text [12712-12723] ("#+begin_src")
                          text [12723-12734] (" emacs-lisp")
                      newLine [12734-12735]
                      blockBody [12735-12926]
                        text [12735-12926] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n(message \\"enabled: %s, another prop: %s, type: %s\\" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop) (type-of my-plist))")
                      newLine [12926-12927]
                      blockFooter [12927-12936]
                        keyword [12927-12936]
                          text [12927-12936] ("#+end_src")
                    newLine [12936-12937]
                    keyword [12937-12948]
                      text [12937-12947] ("#+RESULTS:")
                      text [12947-12948] (" ")
                    newLine [12948-12949]
                    fixedWidth [12949-12992]
                      operator [12949-12951] (": ")
                      text [12951-12992] ("enabled: t, another prop: hey, type: cons")
                    newLine [12992-12993]
                    newLine [12993-12994]
                    bold [12994-13005]
                      operator [12994-12995] ("*")
                      text [12995-13004] ("Изменение")
                      operator [13004-13005] ("*")
                    newLine [13005-13006]
                    newLine [13006-13007]
                    srcBlock [13007-13260]
                      blockHeader [13007-13029]
                        keyword [13007-13029]
                          text [13007-13018] ("#+begin_src")
                          text [13018-13029] (" emacs-lisp")
                      newLine [13029-13030]
                      blockBody [13030-13250]
                        text [13030-13250] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n\\n(plist-put my-plist  :another-prop \\"Wow, i was changed\\")\\n(message \\"enabled: %s, another prop: %s\\" (plist-get my-plist :is-enabled) (plist-get my-plist :another-prop))")
                      newLine [13250-13251]
                      blockFooter [13251-13260]
                        keyword [13251-13260]
                          text [13251-13260] ("#+end_src")
                    newLine [13260-13261]
                    bold [13261-13280]
                      operator [13261-13262] ("*")
                      text [13262-13279] ("Итерация по plist")
                      operator [13279-13280] ("*")
                    newLine [13280-13281]
                    newLine [13281-13282]
                    srcBlock [13282-13659]
                      blockHeader [13282-13304]
                        keyword [13282-13304]
                          text [13282-13293] ("#+begin_src")
                          text [13293-13304] (" emacs-lisp")
                      newLine [13304-13305]
                      blockBody [13305-13649]
                        text [13305-13649] ("(setq my-plist '(:is-enabled t :another-prop \\"hey\\"))\\n\\n(setq res \\"res: \\")\\n(loop for (k v) on my-plist by 'cddr do\\n      (setq res (concat res (format \\"%s - %s\\" k v) \\"\\n\\")))\\n\\n;; (mapcar (lambda (k) (setq res (concat res (format \\"%s - \\" k ) \\"\\n\\"))) my-plist)\\n\\n\\n;; (dolist (p my-plist)\\n;;   (setq res (concat res (format \\"%s\\" p) \\"\\n\\")))\\n\\n(message res)")
                      newLine [13649-13650]
                      blockFooter [13650-13659]
                        keyword [13650-13659]
                          text [13650-13659] ("#+end_src")
                    newLine [13659-13660]
                    bold [13660-13688]
                      operator [13660-13661] ("*")
                      text [13661-13687] ("Удаление элемента из plist")
                      operator [13687-13688] ("*")
                    newLine [13688-13689]
                    newLine [13689-13690]
                    srcBlock [13690-13821]
                      blockHeader [13690-13712]
                        keyword [13690-13712]
                          text [13690-13701] ("#+begin_src")
                          text [13701-13712] (" emacs-lisp")
                      newLine [13712-13713]
                      blockBody [13713-13811]
                        text [13713-13811] ("(setq test '(:hi \\"there\\" :by \\"man!\\"))\\n\\n(setq test (map-delete test :hi))\\n\\n(message \\"res: %s\\" test)")
                      newLine [13811-13812]
                      blockFooter [13812-13821]
                        keyword [13812-13821]
                          text [13812-13821] ("#+end_src")
                    newLine [13821-13822]
                    keyword [13822-13833]
                      text [13822-13832] ("#+RESULTS:")
                      text [13832-13833] (" ")
                    newLine [13833-13834]
                    fixedWidth [13834-13851]
                      operator [13834-13836] (": ")
                      text [13836-13851] ("res: (:by man!)")
                    newLine [13851-13852]
                    newLine [13852-13853]
                    keyword [13853-13870]
                      text [13853-13870] ("#+CLOSE_{SPOILER}")
                    newLine [13870-13871]
                headline [13871-14274]
                    :level 3:
                  title [13871-13987]
                    operator [13871-13875] ("*** ")
                    link [13875-13986]
                      operator [13875-13876] ("[")
                      linkUrl [13876-13958]
                        operator [13876-13877] ("[")
                        text [13877-13957] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Idle-Timers.html")
                        operator [13957-13958] ("]")
                      linkName [13958-13985]
                        operator [13958-13959] ("[")
                        text [13959-13984] ("Отложенный запуск функций")
                        operator [13984-13985] ("]")
                      operator [13985-13986] ("]")
                    newLine [13986-13987]
                  section [13987-14274]
                    srcBlock [13987-14100]
                      blockHeader [13987-14009]
                        keyword [13987-14009]
                          text [13987-13998] ("#+begin_src")
                          text [13998-14009] (" emacs-lisp")
                      newLine [14009-14010]
                      blockBody [14010-14090]
                        text [14010-14090] ("(setq my-custom-timer (run-with-idle-timer 1 nil #'(lambda () (message \\"qwe\\"))))")
                      newLine [14090-14091]
                      blockFooter [14091-14100]
                        keyword [14091-14100]
                          text [14091-14100] ("#+end_src")
                    newLine [14100-14101]
                    keyword [14101-14112]
                      text [14101-14111] ("#+RESULTS:")
                      text [14111-14112] (" ")
                    newLine [14112-14113]
                    fixedWidth [14113-14173]
                      operator [14113-14115] (": ")
                      text [14115-14173] ("[nil 0 1 0 nil (lambda nil 'message \\"qwe\\") nil idle 0 nil]")
                    newLine [14173-14174]
                    newLine [14174-14175]
                    text [14175-14208] ("Отложенные функции можно отменять")
                    newLine [14208-14209]
                    newLine [14209-14210]
                    srcBlock [14210-14273]
                      blockHeader [14210-14232]
                        keyword [14210-14232]
                          text [14210-14221] ("#+begin_src")
                          text [14221-14232] (" emacs-lisp")
                      newLine [14232-14233]
                      blockBody [14233-14263]
                        text [14233-14263] ("(cancel-timer my-custom-timer)")
                      newLine [14263-14264]
                      blockFooter [14264-14273]
                        keyword [14264-14273]
                          text [14264-14273] ("#+end_src")
                    newLine [14273-14274]
            headline [14274-15664]
                :level 2:
              title [14274-14287]
                operator [14274-14277] ("** ")
                text [14277-14286] ("Операторы")
                newLine [14286-14287]
              section [14287-15664]
                text [14287-14393] ("Орпеторы это точно такие же функции. Вынес в отдельную категорию т.к. в большинстве языков это инструкции.")
                newLine [14393-14394]
                keyword [14394-14419]
                  text [14394-14419] ("#+START_{SPOILER} Детали ")
                text [14419-14420] (">")
                newLine [14420-14421]
                newLine [14421-14422]
                headline [14422-14601]
                    :level 3:
                  title [14422-14438]
                    operator [14422-14426] ("*** ")
                    text [14426-14437] ("Switch case")
                    newLine [14437-14438]
                  section [14438-14601]
                    srcBlock [14438-14582]
                      blockHeader [14438-14460]
                        keyword [14438-14460]
                          text [14438-14449] ("#+begin_src")
                          text [14449-14460] (" emacs-lisp")
                      newLine [14460-14461]
                      blockBody [14461-14572]
                        text [14461-14572] ("(setq test-var 'qwe)\\n(message \\"%s\\" (cond ((eq test-var 'q2e) 1)\\n       ((eq test-var 'oe) 2)\\n       (t \\"qwe\\")))")
                      newLine [14572-14573]
                      blockFooter [14573-14582]
                        keyword [14573-14582]
                          text [14573-14582] ("#+end_src")
                    newLine [14582-14583]
                    keyword [14583-14594]
                      text [14583-14593] ("#+RESULTS:")
                      text [14593-14594] (" ")
                    newLine [14594-14595]
                    fixedWidth [14595-14600]
                      operator [14595-14597] (": ")
                      text [14597-14600] ("qwe")
                    newLine [14600-14601]
                headline [14601-14778]
                    :level 3:
                  title [14601-14611]
                    operator [14601-14605] ("*** ")
                    text [14605-14610] ("While")
                    newLine [14610-14611]
                  section [14611-14778]
                    srcBlock [14611-14760]
                      blockHeader [14611-14633]
                        keyword [14611-14633]
                          text [14611-14622] ("#+begin_src")
                          text [14622-14633] (" emacs-lisp")
                      newLine [14633-14634]
                      blockBody [14634-14750]
                        text [14634-14750] ("(setq my-counter 0)\\n(while (< my-counter 12)\\n         (setq my-counter (+ my-counter 1)))\\n\\n(message \\"%s\\" my-counter)")
                      newLine [14750-14751]
                      blockFooter [14751-14760]
                        keyword [14751-14760]
                          text [14751-14760] ("#+end_src")
                    newLine [14760-14761]
                    keyword [14761-14772]
                      text [14761-14771] ("#+RESULTS:")
                      text [14771-14772] (" ")
                    newLine [14772-14773]
                    fixedWidth [14773-14777]
                      operator [14773-14775] (": ")
                      text [14775-14777] ("12")
                    newLine [14777-14778]
                headline [14778-15295]
                    :level 3:
                  title [14778-14788]
                    operator [14778-14782] ("*** ")
                    text [14782-14787] ("Catch")
                    newLine [14787-14788]
                  section [14788-15295]
                    text [14788-15002] ("Просто вау, в фп есть try catch! Я действительно удивлен..даже в объектно ориетированых языках это вызывает проблемы..тем не менее..это 1 из вариантов прерываия цикла while (плохихи вариатов, как по мне, но все же)")
                    newLine [15002-15003]
                    newLine [15003-15004]
                    srcBlock [15004-15294]
                      blockHeader [15004-15026]
                        keyword [15004-15026]
                          text [15004-15015] ("#+begin_src")
                          text [15015-15026] (" emacs-lisp")
                      newLine [15026-15027]
                      blockBody [15027-15284]
                        text [15027-15284] ("(setq my-counter 0)\\n\\n\\n(message \\"What is the messafe from catch? Oh this is message: %s\\" (catch 'result\\n  (while (< my-counter 22)\\n    (setq my-counter (+ my-counter 1))\\n    (if (> my-counter 5)\\n        (throw 'result \\"Amma result from catch block\\"))\\n    )))")
                      newLine [15284-15285]
                      blockFooter [15285-15294]
                        keyword [15285-15294]
                          text [15285-15294] ("#+end_src")
                    newLine [15294-15295]
                headline [15295-15664]
                    :level 3:
                  title [15295-15306]
                    operator [15295-15299] ("*** ")
                    text [15299-15305] ("Return")
                    newLine [15305-15306]
                  section [15306-15664]
                    text [15306-15317] ("Работает в ")
                    bold [15317-15330]
                      operator [15317-15318] ("*")
                      text [15318-15329] ("emacs 27.1+")
                      operator [15329-15330] ("*")
                    text [15330-15370] (". Позволяет прервать выполнение функции.")
                    newLine [15370-15371]
                    newLine [15371-15372]
                    srcBlock [15372-15628]
                      blockHeader [15372-15394]
                        keyword [15372-15394]
                          text [15372-15383] ("#+begin_src")
                          text [15383-15394] (" emacs-lisp")
                      newLine [15394-15395]
                      blockBody [15395-15618]
                        text [15395-15618] ("(setq my-counter 0)\\n(cl-defun my-iterator()\\n  (while (< my-counter 12)\\n    (if (> my-counter 3)\\n        (return-from my-iterator)\\n      )\\n    (setq my-counter (+ my-counter 1)))\\n  )\\n\\n(my-iterator)\\n\\n(message \\"%s\\" my-counter)")
                      newLine [15618-15619]
                      blockFooter [15619-15628]
                        keyword [15619-15628]
                          text [15619-15628] ("#+end_src")
                    newLine [15628-15629]
                    keyword [15629-15640]
                      text [15629-15639] ("#+RESULTS:")
                      text [15639-15640] (" ")
                    newLine [15640-15641]
                    fixedWidth [15641-15644]
                      operator [15641-15643] (": ")
                      text [15643-15644] ("4")
                    newLine [15644-15645]
                    newLine [15645-15646]
                    keyword [15646-15663]
                      text [15646-15663] ("#+CLOSE_{SPOILER}")
                    newLine [15663-15664]
            headline [15664-20337]
                :level 2:
              title [15664-15690]
                operator [15664-15667] ("** ")
                text [15667-15689] ("Взаимодействие с emacs")
                newLine [15689-15690]
              section [15690-20337]
                keyword [15690-15715]
                  text [15690-15715] ("#+START_{SPOILER} Детали ")
                text [15715-15716] (">")
                newLine [15716-15717]
                newLine [15717-15718]
                headline [15718-15801]
                    :level 3:
                  title [15718-15739]
                    operator [15718-15722] ("*** ")
                    text [15722-15738] ("Вставка в текста")
                    newLine [15738-15739]
                  section [15739-15801]
                    srcBlock [15739-15800]
                      blockHeader [15739-15761]
                        keyword [15739-15761]
                          text [15739-15750] ("#+begin_src")
                          text [15750-15761] (" emacs-lisp")
                      newLine [15761-15762]
                      blockBody [15762-15790]
                        text [15762-15790] ("(insert \\"Hello\\" \\" \\" \\"World\\")")
                      newLine [15790-15791]
                      blockFooter [15791-15800]
                        keyword [15791-15800]
                          text [15791-15800] ("#+end_src")
                    newLine [15800-15801]
                headline [15801-16384]
                    :level 3:
                  title [15801-15822]
                    operator [15801-15805] ("*** ")
                    text [15805-15821] ("Работа с буфером")
                    newLine [15821-15822]
                  section [15822-16384]
                    headline [15822-15997]
                        :level 4:
                      title [15822-15862]
                        operator [15822-15827] ("**** ")
                        text [15827-15861] ("Программное создание нового буфера")
                        newLine [15861-15862]
                      section [15862-15997]
                        srcBlock [15862-15996]
                          blockHeader [15862-15884]
                            keyword [15862-15884]
                              text [15862-15873] ("#+begin_src")
                              text [15873-15884] (" emacs-lisp")
                          newLine [15884-15885]
                          blockBody [15885-15986]
                            text [15885-15986] ("  (switch-to-buffer-other-window \\"*my-first-buffer*\\")\\n  (insert \\"Congratulations! I'am a new buffer\\")")
                          newLine [15986-15987]
                          blockFooter [15987-15996]
                            keyword [15987-15996]
                              text [15987-15996] ("#+end_src")
                        newLine [15996-15997]
                    headline [15997-16065]
                        :level 4:
                      title [15997-16017]
                        operator [15997-16002] ("**** ")
                        text [16002-16016] ("Очистка буфера")
                        newLine [16016-16017]
                      section [16017-16065]
                        srcBlock [16017-16064]
                          blockHeader [16017-16039]
                            keyword [16017-16039]
                              text [16017-16028] ("#+begin_src")
                              text [16028-16039] (" emacs-lisp")
                          newLine [16039-16040]
                          blockBody [16040-16054]
                            text [16040-16054] ("(erase-buffer)")
                          newLine [16054-16055]
                          blockFooter [16055-16064]
                            keyword [16055-16064]
                              text [16055-16064] ("#+end_src")
                        newLine [16064-16065]
                    headline [16065-16384]
                        :level 4:
                      title [16065-16089]
                        operator [16065-16070] ("**** ")
                        text [16070-16088] ("Интерактивный ввод")
                        newLine [16088-16089]
                      section [16089-16384]
                        srcBlock [16089-16372]
                          blockHeader [16089-16111]
                            keyword [16089-16111]
                              text [16089-16100] ("#+begin_src")
                              text [16100-16111] (" emacs-lisp")
                          newLine [16111-16112]
                          blockBody [16112-16362]
                            text [16112-16362] ("  ;; (read-from-minibuffer \\"Enter your name: \\")\\n  (let ((your-name (read-from-minibuffer \\"Enter your name: \\")))\\n      (switch-to-buffer-other-window \\"*Your personal info\\")\\n  (erase-buffer)\\n  (insert (format \\"Hello %s!\\" your-name))\\n  (other-window 1))")
                          newLine [16362-16363]
                          blockFooter [16363-16372]
                            keyword [16363-16372]
                              text [16363-16372] ("#+end_src")
                        newLine [16372-16373]
                        keyword [16373-16383]
                          text [16373-16383] ("#+RESULTS:")
                        newLine [16383-16384]
                headline [16384-17784]
                    :level 3:
                  title [16384-16405]
                    operator [16384-16388] ("*** ")
                    text [16388-16404] ("Replace в буфере")
                    newLine [16404-16405]
                  section [16405-17784]
                    srcBlock [16405-16829]
                      blockHeader [16405-16427]
                        keyword [16405-16427]
                          text [16405-16416] ("#+begin_src")
                          text [16416-16427] (" emacs-lisp")
                      newLine [16427-16428]
                      blockBody [16428-16819]
                        text [16428-16819] ("  (defun detect-bad-boys ()\\n    (setq lesson-list '(\\"Buzova\\" \\"Volodin\\" \\"Pupin\\"))\\n  \\n    (defun mark-as-bad (name)\\n      (insert (format \\"Bad boy %s \\n\\" name)))\\n  \\n    (switch-to-buffer-other-window \\"*lisp lesson*\\")\\n    (mapcar 'mark-as-bad lesson-list)\\n    (goto-char (point-min))\\n    (while (search-forward \\"Bad\\")\\n      (replace-match \\"Awful\\"))\\n    (other-window 1)\\n    )\\n  (detect-bad-boys)")
                      newLine [16819-16820]
                      blockFooter [16820-16829]
                        keyword [16820-16829]
                          text [16820-16829] ("#+end_src")
                    newLine [16829-16830]
                    bold [16830-16841]
                      operator [16830-16831] ("*")
                      text [16831-16840] ("goto-char")
                      operator [16840-16841] ("*")
                    text [16841-16842] (" ")
                    list [16842-16890]
                        :unordered:
                        :level 0:
                      listItem [16842-16874]
                        title [16842-16874]
                          operator [16842-16844] ("- ")
                          text [16844-16873] ("переход к конкретному символу")
                          newLine [16873-16874]
                      listItem [16874-16890]
                        title [16874-16890]
                          operator [16874-16876] ("- ")
                          text [16876-16889] ("начало буфера")
                          newLine [16889-16890]
                    bold [16890-16901]
                      operator [16890-16891] ("*")
                      text [16891-16900] ("point-min")
                      operator [16900-16901] ("*")
                    text [16901-16902] (" ")
                    headline [16902-17381]
                        :level 3:
                      title [16902-16936]
                        operator [16902-16906] ("*** ")
                        text [16906-16935] ("Добавление свойств для текста")
                        newLine [16935-16936]
                      section [16936-17381]
                        indent [16936-16938] ("  ")
                        text [16938-16958] (";; (detect-bad-boys)")
                        newLine [16958-16959]
                        indent [16959-16961] ("  ")
                        newLine [16961-16962]
                        indent [16962-16964] ("  ")
                        newLine [16964-16965]
                        indent [16965-16967] ("  ")
                        text [16967-16993] ("(defun boldify-bad-boys ()")
                        newLine [16993-16994]
                        indent [16994-16998] ("    ")
                        text [16998-17030] ("(switch-to-buffer-other-window \\"")
                        bold [17030-17043]
                          operator [17030-17031] ("*")
                          text [17031-17042] ("lisp lesson")
                          operator [17042-17043] ("*")
                        text [17043-17045] ("\\")")
                        newLine [17045-17046]
                        indent [17046-17050] ("    ")
                        text [17050-17073] ("(goto-char (point-min))")
                        newLine [17073-17074]
                        indent [17074-17078] ("    ")
                        text [17078-17115] ("(while (re-search-forward \\"Awful boy ")
                        keyword [17115-17118]
                          text [17115-17118] ("\\\\(.")
                        text [17118-17119] ("+")
                        keyword [17119-17129]
                          text [17119-17122] ("\\\\)\\"")
                          text [17122-17129] (" nil t)")
                        newLine [17129-17130]
                        indent [17130-17136] ("      ")
                        text [17136-17183] ("(message (format \\"Its %s\\" (match-beginning 1)))")
                        newLine [17183-17184]
                        indent [17184-17190] ("      ")
                        text [17190-17230] ("(add-text-properties (match-beginning 1)")
                        newLine [17230-17231]
                        indent [17231-17258] ("                           ")
                        text [17258-17271] ("(match-end 1)")
                        newLine [17271-17272]
                        indent [17272-17299] ("                           ")
                        text [17299-17326] ("(list 'face 'bold-italic)))")
                        newLine [17326-17327]
                        indent [17327-17331] ("    ")
                        text [17331-17350] (";; (other-window 1)")
                        newLine [17350-17351]
                        indent [17351-17355] ("    ")
                        text [17355-17356] (")")
                        newLine [17356-17357]
                        indent [17357-17359] ("  ")
                        newLine [17359-17360]
                        indent [17360-17362] ("  ")
                        text [17362-17380] ("(boldify-bad-boys)")
                        newLine [17380-17381]
                    italic [17381-17433]
                      operator [17381-17382] ("/")
                      text [17382-17432] ("Перед этим необходимо запустить предыдущую функцию")
                      operator [17432-17433] ("/")
                    newLine [17433-17434]
                    newLine [17434-17435]
                    srcBlock [17435-17467]
                      blockHeader [17435-17457]
                        keyword [17435-17457]
                          text [17435-17446] ("#+begin_src")
                          text [17446-17457] (" emacs-lisp")
                      newLine [17457-17458]
                      blockFooter [17458-17467]
                        keyword [17458-17467]
                          text [17458-17467] ("#+end_src")
                    newLine [17467-17468]
                    keyword [17468-17479]
                      text [17468-17478] ("#+RESULTS:")
                      text [17478-17479] (" ")
                    newLine [17479-17480]
                    text [17480-17504] ("Про сумасшедшие регекспы")
                    newLine [17504-17505]
                    newLine [17505-17506]
                    quoteBlock [17506-17783]
                      blockHeader [17506-17519]
                        keyword [17506-17519]
                          text [17506-17519] ("#+begin_quote")
                      newLine [17519-17520]
                      blockBody [17520-17771]
                        text [17520-17558] (";; The regular expression is \\"Bonjour ")
                        keyword [17558-17561]
                          text [17558-17561] ("\\\\(.")
                        text [17561-17562] ("+")
                        keyword [17562-17580]
                          text [17562-17566] ("\\\\)!\\"")
                          text [17566-17580] (" and it reads:")
                        newLine [17580-17581]
                        text [17581-17610] (";; the string \\"Bonjour \\", and")
                        newLine [17610-17611]
                        text [17611-17649] (";; a group of           | this is the ")
                        keyword [17649-17656]
                          text [17649-17651] ("\\\\(")
                          text [17651-17656] (" ... ")
                        keyword [17656-17668]
                          text [17656-17658] ("\\\\)")
                          text [17658-17668] (" construct")
                        newLine [17668-17669]
                        text [17669-17708] (";;   any character      | this is the .")
                        newLine [17708-17709]
                        text [17709-17748] (";;   possibly repeated  | this is the +")
                        newLine [17748-17749]
                        text [17749-17771] (";; and the \\"!\\" string.")
                      newLine [17771-17772]
                      blockFooter [17772-17783]
                        keyword [17772-17783]
                          text [17772-17783] ("#+end_quote")
                    newLine [17783-17784]
                headline [17784-18439]
                    :level 3:
                  title [17784-17806]
                    operator [17784-17788] ("*** ")
                    text [17788-17805] ("Создание кнопочки")
                    newLine [17805-17806]
                  section [17806-18439]
                    text [17806-17868] ("Даннный метод создает кнопку над текстом с позиции от 1 до 10.")
                    newLine [17868-17869]
                    newLine [17869-17870]
                    srcBlock [17870-18178]
                      blockHeader [17870-17892]
                        keyword [17870-17892]
                          text [17870-17881] ("#+begin_src")
                          text [17881-17892] (" emacs-lisp")
                      newLine [17892-17893]
                      blockBody [17893-18168]
                        text [17893-18168] ("(defun butest-varon-pressed (button)\\n  (message (format \\"Butest-varon pressed!\\")))\\n\\n(define-butest-varon-type 'custom-button\\n  'action 'butest-varon-pressed\\n  'follow-link t\\n  'help-echo \\"Click Butest-varon\\"\\n  'help-args \\"test\\")\\n\\n(make-butest-varon 1 10 :type 'custom-button)")
                      newLine [18168-18169]
                      blockFooter [18169-18178]
                        keyword [18169-18178]
                          text [18169-18178] ("#+end_src")
                    newLine [18178-18179]
                    text [18179-18239] ("Данная функция вставляет кнопку под текущей позицей каретки.")
                    newLine [18239-18240]
                    newLine [18240-18241]
                    srcBlock [18241-18387]
                      blockHeader [18241-18263]
                        keyword [18241-18263]
                          text [18241-18252] ("#+begin_src")
                          text [18252-18263] (" emacs-lisp")
                      newLine [18263-18264]
                      blockBody [18264-18377]
                        text [18264-18377] ("(insert-butest-varon \\"Press me\\"\\n               'action (lambda (_arg) (print \\"You are press the butest-varon!\\")))")
                      newLine [18377-18378]
                      blockFooter [18378-18387]
                        keyword [18378-18387]
                          text [18378-18387] ("#+end_src")
                    newLine [18387-18388]
                    keyword [18388-18399]
                      text [18388-18398] ("#+RESULTS:")
                      text [18398-18399] (" ")
                    newLine [18399-18400]
                    fixedWidth [18400-18438]
                      operator [18400-18402] (": ")
                      text [18402-18438] ("#<overlay from 1 to 10 in elisp.org>")
                    newLine [18438-18439]
                headline [18439-18551]
                    :level 3:
                  title [18439-18464]
                    operator [18439-18443] ("*** ")
                    text [18443-18463] ("Чтение из completion")
                    newLine [18463-18464]
                  section [18464-18551]
                    srcBlock [18464-18550]
                      blockHeader [18464-18486]
                        keyword [18464-18486]
                          text [18464-18475] ("#+begin_src")
                          text [18475-18486] (" emacs-lisp")
                      newLine [18486-18487]
                      blockBody [18487-18540]
                        text [18487-18540] ("(completing-read \\"Choose one: \\" '(\\"foo\\" \\"bar\\" \\"baz\\"))")
                      newLine [18540-18541]
                      blockFooter [18541-18550]
                        keyword [18541-18550]
                          text [18541-18550] ("#+end_src")
                    newLine [18550-18551]
                headline [18551-18706]
                    :level 3:
                  title [18551-18577]
                    operator [18551-18555] ("*** ")
                    text [18555-18576] ("Пользовательский ввод")
                    newLine [18576-18577]
                  section [18577-18706]
                    srcBlock [18577-18666]
                      blockHeader [18577-18599]
                        keyword [18577-18599]
                          text [18577-18588] ("#+begin_src")
                          text [18588-18599] (" emacs-lisp")
                      newLine [18599-18600]
                      blockBody [18600-18656]
                        text [18600-18656] ("(message \\"U say: %s\\" (read-string \\"Say me something: \\"))")
                      newLine [18656-18657]
                      blockFooter [18657-18666]
                        keyword [18657-18666]
                          text [18657-18666] ("#+end_src")
                    newLine [18666-18667]
                    keyword [18667-18678]
                      text [18667-18677] ("#+RESULTS:")
                      text [18677-18678] (" ")
                    newLine [18678-18679]
                    fixedWidth [18679-18705]
                      operator [18679-18681] (": ")
                      text [18681-18705] ("U say: Ну че тут скажешь")
                    newLine [18705-18706]
                headline [18706-18892]
                    :level 3:
                  title [18706-18738]
                    operator [18706-18710] ("*** ")
                    text [18710-18737] ("Работа с выделенным текстом")
                    newLine [18737-18738]
                  section [18738-18892]
                    headline [18738-18789]
                        :level 4:
                      title [18738-18772]
                        operator [18738-18743] ("**** ")
                        text [18743-18771] ("Проверка что что-то выделено")
                        newLine [18771-18772]
                      section [18772-18789]
                        verbatim [18772-18788]
                          operator [18772-18773] ("=")
                          text [18773-18787] ("(use-region-p)")
                          operator [18787-18788] ("=")
                        newLine [18788-18789]
                    headline [18789-18892]
                        :level 4:
                      title [18789-18820]
                        operator [18789-18794] ("**** ")
                        text [18794-18819] ("Получить выделенный текст")
                        newLine [18819-18820]
                      section [18820-18892]
                        srcBlock [18820-18891]
                          blockHeader [18820-18842]
                            keyword [18820-18842]
                              text [18820-18831] ("#+begin_src")
                              text [18831-18842] (" emacs-lisp")
                          newLine [18842-18843]
                          blockBody [18843-18881]
                            text [18843-18881] ("(regionp (buffer-substring start end))")
                          newLine [18881-18882]
                          blockFooter [18882-18891]
                            keyword [18882-18891]
                              text [18882-18891] ("#+end_src")
                        newLine [18891-18892]
                headline [18892-19111]
                    :level 3:
                  title [18892-18938]
                    operator [18892-18896] ("*** ")
                    text [18896-18937] ("Конвертация символа в строку (ну и назад)")
                    newLine [18937-18938]
                  section [18938-19111]
                    srcBlock [18938-19067]
                      blockHeader [18938-18960]
                        keyword [18938-18960]
                          text [18938-18949] ("#+begin_src")
                          text [18949-18960] (" emacs-lisp")
                      newLine [18960-18961]
                      blockBody [18961-19057]
                        text [18961-19057] ("(symbol-name 'something) ;; Символ в строку\\n(intern (symbol-name 'something)) ;; Строка в символ")
                      newLine [19057-19058]
                      blockFooter [19058-19067]
                        keyword [19058-19067]
                          text [19058-19067] ("#+end_src")
                    newLine [19067-19068]
                    keyword [19068-19079]
                      text [19068-19078] ("#+RESULTS:")
                      text [19078-19079] (" ")
                    newLine [19079-19080]
                    fixedWidth [19080-19091]
                      operator [19080-19082] (": ")
                      text [19082-19091] ("something")
                    newLine [19091-19092]
                    newLine [19092-19093]
                    keyword [19093-19110]
                      text [19093-19110] ("#+CLOSE_{SPOILER}")
                    newLine [19110-19111]
                headline [19111-20192]
                    :level 3:
                  title [19111-19123]
                    operator [19111-19115] ("*** ")
                    text [19115-19122] ("Overlay")
                    newLine [19122-19123]
                  section [19123-20192]
                    text [19123-19303] ("Overlay это очень крутая тема. Он позволяет рендерить текст который не изменяет контент реального буфера. Это может быть полезно для показа подсказок, дебага, расчитанных значений.")
                    newLine [19303-19304]
                    newLine [19304-19305]
                    headline [19305-19454]
                        :level 4:
                      title [19305-19342]
                        operator [19305-19310] ("**** ")
                        text [19310-19341] ("Создание оверлея в конце строки")
                        newLine [19341-19342]
                      section [19342-19454]
                        srcBlock [19342-19453]
                          blockHeader [19342-19364]
                            keyword [19342-19364]
                              text [19342-19353] ("#+begin_src")
                              text [19353-19364] (" emacs-lisp")
                          newLine [19364-19365]
                          blockBody [19365-19443]
                            text [19365-19443] ("(setq my-first-overlay (make-overlay (line-end-position) (line-end-position)))")
                          newLine [19443-19444]
                          blockFooter [19444-19453]
                            keyword [19444-19453]
                              text [19444-19453] ("#+end_src")
                        newLine [19453-19454]
                    headline [19454-19749]
                        :level 4:
                      title [19454-19492]
                        operator [19454-19459] ("**** ")
                        text [19459-19491] ("Курсор заходит за предел оверлея")
                        newLine [19491-19492]
                      section [19492-19749]
                        text [19492-19622] ("В моем случае курсор выходил за предел оверлея. Решается весьма просто: вставляемый в оверлей текст необходимо наделить свойством ")
                        verbatim [19622-19633]
                          operator [19622-19623] ("=")
                          text [19623-19632] ("'cursor t")
                          operator [19632-19633] ("=")
                        newLine [19633-19634]
                        newLine [19634-19635]
                        srcBlock [19635-19748]
                          blockHeader [19635-19657]
                            keyword [19635-19657]
                              text [19635-19646] ("#+begin_src")
                              text [19646-19657] (" emacs-lisp")
                          newLine [19657-19658]
                          blockBody [19658-19738]
                            text [19658-19738] ("(setq my-popup-message (propertize popup-message 'face 'blamer--face 'cursor t))")
                          newLine [19738-19739]
                          blockFooter [19739-19748]
                            keyword [19739-19748]
                              text [19739-19748] ("#+end_src")
                        newLine [19748-19749]
                    headline [19749-20059]
                        :level 4:
                      title [19749-19780]
                        operator [19749-19754] ("**** ")
                        text [19754-19779] ("Изменение свойств overlay")
                        newLine [19779-19780]
                      section [19780-20059]
                        srcBlock [19780-20058]
                          blockHeader [19780-19802]
                            keyword [19780-19802]
                              text [19780-19791] ("#+begin_src")
                              text [19791-19802] (" emacs-lisp")
                          newLine [19802-19803]
                          blockBody [19803-20048]
                            text [19803-20048] ("    (overlay-put blamer--current-overlay 'after-string my-popup-message)\\n    (overlay-put blamer--current-overlay 'intangible t)\\n    (overlay-put blamer--current-overlay 'face 'bold)\\n    (overlay-put blamer--current-overlay 'cursor-intangible t)")
                          newLine [20048-20049]
                          blockFooter [20049-20058]
                            keyword [20049-20058]
                              text [20049-20058] ("#+end_src")
                        newLine [20058-20059]
                    headline [20059-20192]
                        :level 4:
                      title [20059-20095]
                        operator [20059-20064] ("**** ")
                        text [20064-20094] ("Удаление существующего оверлея")
                        newLine [20094-20095]
                      section [20095-20192]
                        srcBlock [20095-20191]
                          blockHeader [20095-20117]
                            keyword [20095-20117]
                              text [20095-20106] ("#+begin_src")
                              text [20106-20117] (" emacs-lisp")
                          newLine [20117-20118]
                          blockBody [20118-20181]
                            text [20118-20181] ("(if my-first-overlay\\n        (delete-overlay my-first-overlay))")
                          newLine [20181-20182]
                          blockFooter [20182-20191]
                            keyword [20182-20191]
                              text [20182-20191] ("#+end_src")
                        newLine [20191-20192]
                headline [20192-20337]
                    :level 3:
                  title [20192-20229]
                    operator [20192-20196] ("*** ")
                    text [20196-20223] ("Создание своего minor-mode ")
                    tagList [20223-20228]
                      operator [20223-20224] (":")
                      text [20224-20227] ("WIP")
                      operator [20227-20228] (":")
                    newLine [20228-20229]
                  section [20229-20337]
                    link [20229-20336]
                      operator [20229-20230] ("[")
                      linkUrl [20230-20321]
                        operator [20230-20231] ("[")
                        text [20231-20320] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Minor-Modes.html")
                        operator [20320-20321] ("]")
                      linkName [20321-20335]
                        operator [20321-20322] ("[")
                        text [20322-20334] ("Документация")
                        operator [20334-20335] ("]")
                      operator [20335-20336] ("]")
                    newLine [20336-20337]
            headline [20337-20726]
                :level 2:
              title [20337-20356]
                operator [20337-20340] ("** ")
                text [20340-20355] ("Работа с датами")
                newLine [20355-20356]
              section [20356-20726]
                link [20356-20481]
                  operator [20356-20357] ("[")
                  linkUrl [20357-20453]
                    operator [20357-20358] ("[")
                    text [20358-20452] ("htest-varps://stackoverflow.com/questions/4242012/how-do-i-add-dates-in-emacs-using-emacs-lisp")
                    operator [20452-20453] ("]")
                  linkName [20453-20480]
                    operator [20453-20454] ("[")
                    text [20454-20479] ("Ага, любимый стаковерфлоу")
                    operator [20479-20480] ("]")
                  operator [20480-20481] ("]")
                newLine [20481-20482]
                newLine [20482-20483]
                srcBlock [20483-20573]
                  blockHeader [20483-20505]
                    keyword [20483-20505]
                      text [20483-20494] ("#+begin_src")
                      text [20494-20505] (" emacs-lisp")
                  newLine [20505-20506]
                  blockBody [20506-20563]
                    text [20506-20563] ("(setq t3 (time-subtract (current-time) (days-to-time 2)))")
                  newLine [20563-20564]
                  blockFooter [20564-20573]
                    keyword [20564-20573]
                      text [20564-20573] ("#+end_src")
                newLine [20573-20574]
                table [20574-20604]
                  tableRow [20574-20603]
                    operator [20574-20575] ("|")
                    tableCell [20575-20582]
                      text [20575-20582] (" 24939 ")
                    operator [20582-20583] ("|")
                    tableCell [20583-20589]
                      text [20583-20589] (" 1255 ")
                    operator [20589-20590] ("|")
                    tableCell [20590-20598]
                      text [20590-20598] (" 721279 ")
                    operator [20598-20599] ("|")
                    tableCell [20599-20602]
                      text [20599-20602] (" 0 ")
                    operator [20602-20603] ("|")
                  newLine [20603-20604]
                srcBlock [20604-20692]
                  blockHeader [20604-20626]
                    keyword [20604-20626]
                      text [20604-20615] ("#+begin_src")
                      text [20615-20626] (" emacs-lisp")
                  newLine [20626-20627]
                  blockBody [20627-20682]
                    text [20627-20682] ("\\n(message \\"%s\\" (/ (float-time (time-since t3)) (* 60)))")
                  newLine [20682-20683]
                  blockFooter [20683-20692]
                    keyword [20683-20692]
                      text [20683-20692] ("#+end_src")
                newLine [20692-20693]
                keyword [20693-20704]
                  text [20693-20703] ("#+RESULTS:")
                  text [20703-20704] (" ")
                newLine [20704-20705]
                fixedWidth [20705-20725]
                  operator [20705-20707] (": ")
                  text [20707-20725] ("2940.0710639333333")
                newLine [20725-20726]
            headline [20726-21752]
                :level 2:
              title [20726-20736]
                operator [20726-20729] ("** ")
                text [20729-20735] ("Regexp")
                newLine [20735-20736]
              section [20736-21752]
                headline [20736-21397]
                    :level 3:
                  title [20736-20748]
                    operator [20736-20740] ("*** ")
                    text [20740-20747] ("Примеры")
                    newLine [20747-20748]
                  section [20748-21397]
                    text [20748-20793] ("Просто кучка примеров из разработанного мной ")
                    link [20793-20849]
                      operator [20793-20794] ("[")
                      linkUrl [20794-20840]
                        operator [20794-20795] ("[")
                        text [20795-20839] ("htest-varps://github.com/Artawower/turbo-log")
                        operator [20839-20840] ("]")
                      linkName [20840-20848]
                        operator [20840-20841] ("[")
                        text [20841-20847] ("пакета")
                        operator [20847-20848] ("]")
                      operator [20848-20849] ("]")
                    text [20849-21025] (". Регекспы весьма похожи на то что представлено в других языках. Сложно лишь работать с интерполяцией строк (неочевидна работа с большим количеством слешей в исполняемом коде.)")
                    newLine [21025-21026]
                    newLine [21026-21027]
                    srcBlock [21027-21118]
                      blockHeader [21027-21049]
                        keyword [21027-21049]
                          text [21027-21038] ("#+begin_src")
                          text [21038-21049] (" emacs-lisp")
                      newLine [21049-21050]
                      blockBody [21050-21108]
                        text [21050-21108] ("(string-match \\"^\\\\(\\\\)*\\\\(return\\\\)\\" \\"  return {\\n  name: 2\\n}\\")")
                      newLine [21108-21109]
                      blockFooter [21109-21118]
                        keyword [21109-21118]
                          text [21109-21118] ("#+end_src")
                    newLine [21118-21119]
                    keyword [21119-21130]
                      text [21119-21129] ("#+RESULTS:")
                      text [21129-21130] (" ")
                    newLine [21130-21131]
                    fixedWidth [21131-21134]
                      operator [21131-21133] (": ")
                      text [21133-21134] ("0")
                    newLine [21134-21135]
                    newLine [21135-21136]
                    newLine [21136-21137]
                    srcBlock [21137-21257]
                      blockHeader [21137-21159]
                        keyword [21137-21159]
                          text [21137-21148] ("#+begin_src")
                          text [21148-21159] (" emacs-lisp")
                      newLine [21159-21160]
                      blockBody [21160-21247]
                        text [21160-21247] ("(replace-regexp-in-string \\"*=[[:blank:]]*.+\\" \\"\\" \\"    this.myVariable = somethingElse;\\")")
                      newLine [21247-21248]
                      blockFooter [21248-21257]
                        keyword [21248-21257]
                          text [21248-21257] ("#+end_src")
                    newLine [21257-21258]
                    srcBlock [21258-21365]
                      blockHeader [21258-21280]
                        keyword [21258-21280]
                          text [21258-21269] ("#+begin_src")
                          text [21269-21280] (" emacs-lisp")
                      newLine [21280-21281]
                      blockBody [21281-21355]
                        text [21281-21355] ("(replace-regexp-in-string \\"\\\\(const\\\\|let\\\\|public\\\\|protected\\\\|private\\\\|var\\\\)")
                      newLine [21355-21356]
                      blockFooter [21356-21365]
                        keyword [21356-21365]
                          text [21356-21365] ("#+end_src")
                    newLine [21365-21366]
                    keyword [21366-21377]
                      text [21366-21376] ("#+RESULTS:")
                      text [21376-21377] (" ")
                    newLine [21377-21378]
                    fixedWidth [21378-21396]
                      operator [21378-21380] (": ")
                      text [21380-21396] ("iable = userName")
                    newLine [21396-21397]
                headline [21397-21752]
                    :level 3:
                  title [21397-21423]
                    operator [21397-21401] ("*** ")
                    text [21401-21422] ("Regexp с группировкой")
                    newLine [21422-21423]
                  section [21423-21752]
                    srcBlock [21423-21563]
                      blockHeader [21423-21445]
                        keyword [21423-21445]
                          text [21423-21434] ("#+begin_src")
                          text [21434-21445] (" emacs-lisp")
                      newLine [21445-21446]
                      blockBody [21446-21553]
                        text [21446-21553] ("(concat \\"^(?\\\\(?1:\\\\) [^s]\\n]+\\\\)\\"\\n          \\"s\\\\(?3:-[0-9]-[0-9]\\\\)\\"\\n          \\"s\\\\(?4:\\\\{2\\\\}:[0-9]\\\\{2\\\\}:[0-9]\\\\)\\")")
                      newLine [21553-21554]
                      blockFooter [21554-21563]
                        keyword [21554-21563]
                          text [21554-21563] ("#+end_src")
                    newLine [21563-21564]
                    srcBlock [21564-21733]
                      blockHeader [21564-21586]
                        keyword [21564-21586]
                          text [21564-21575] ("#+begin_src")
                          text [21575-21586] (" emacs-lisp")
                      newLine [21586-21587]
                      blockBody [21587-21723]
                        text [21587-21723] ("(setq test-string \\"feature/VW-221\\")\\n(string-match \\"\\\\(?1:+/\\\\)\\\\(?2:VW-[0-9]+\\\\)\\" test-string)\\n(message \\"res \\" (match-string 1 test-string))")
                      newLine [21723-21724]
                      blockFooter [21724-21733]
                        keyword [21724-21733]
                          text [21724-21733] ("#+end_src")
                    newLine [21733-21734]
                    keyword [21734-21745]
                      text [21734-21744] ("#+RESULTS:")
                      text [21744-21745] (" ")
                    newLine [21745-21746]
                    fixedWidth [21746-21751]
                      operator [21746-21748] (": ")
                      text [21748-21751] ("res")
                    newLine [21751-21752]
            headline [21752-21852]
                :level 2:
              title [21752-21772]
                operator [21752-21755] ("** ")
                text [21755-21771] ("Стандартные хуки")
                newLine [21771-21772]
              section [21772-21852]
                link [21772-21851]
                  operator [21772-21773] ("[")
                  linkUrl [21773-21830]
                    operator [21773-21774] ("[")
                    text [21774-21829] ("htest-varps://runebook.dev/ru/docs/elisp/standard-hooks")
                    operator [21829-21830] ("]")
                  linkName [21830-21850]
                    operator [21830-21831] ("[")
                    text [21831-21849] ("Просто смотри сюда")
                    operator [21849-21850] ("]")
                  operator [21850-21851] ("]")
                newLine [21851-21852]
            headline [21852-23084]
                :level 2:
              title [21852-21868]
                operator [21852-21855] ("** ")
                text [21855-21867] ("Custom modes")
                newLine [21867-21868]
              section [21868-23084]
                headline [21868-23084]
                    :level 3:
                  title [21868-21883]
                    operator [21868-21872] ("*** ")
                    text [21872-21882] ("Minor mode")
                    newLine [21882-21883]
                  section [21883-23084]
                    text [21883-21990] ("Для того чтобы сделать свой minor mode достаточно его объявить и описать логику включения/выключений режима")
                    newLine [21990-21991]
                    newLine [21991-21992]
                    srcBlock [21992-22337]
                      blockHeader [21992-22014]
                        keyword [21992-22014]
                          text [21992-22003] ("#+begin_src")
                          text [22003-22014] (" emacs-lisp")
                      newLine [22014-22015]
                      blockBody [22015-22327]
                        text [22015-22327] (";;;###autoload\\n(define-minor-mode wakatime-ui-mode\\n  \\"Wakatime ui mode. Add time track to doom modeline.\\nTODO:\\nAdd support for other modeline in future.\\"\\n  :init-value nil\\n  :global t\\n  :lighter nil\\n  :group 'wakatime-ui\\n  (if wakatime-ui-mode\\n      (wakatime-ui--watch-time)\\n    (wakatime-ui--stop-watch-time)))")
                      newLine [22327-22328]
                      blockFooter [22328-22337]
                        keyword [22328-22337]
                          text [22328-22337] ("#+end_src")
                    newLine [22337-22338]
                    text [22338-22342] ("Где:")
                    newLine [22342-22343]
                    newLine [22343-22344]
                    verbatim [22344-22356]
                      operator [22344-22345] ("=")
                      text [22345-22355] ("init-value")
                      operator [22355-22356] ("=")
                    text [22356-22357] (" ")
                    list [22357-22494]
                        :unordered:
                        :level 0:
                      listItem [22357-22381]
                        title [22357-22381]
                          operator [22357-22359] ("- ")
                          text [22359-22380] ("значение по умолчанию")
                          newLine [22380-22381]
                      listItem [22381-22437]
                        title [22381-22437]
                          operator [22381-22383] ("- ")
                          text [22383-22436] ("должен ли быть вызван глобальный мод перед локальным?")
                          newLine [22436-22437]
                      listItem [22437-22494]
                        title [22437-22494]
                          operator [22437-22439] ("- ")
                          text [22439-22493] ("определяет что отображать в modeline когда мод включен")
                          newLine [22493-22494]
                    verbatim [22494-22502]
                      operator [22494-22495] ("=")
                      text [22495-22501] ("global")
                      operator [22501-22502] ("=")
                    text [22502-22503] (" ")
                    verbatim [22503-22512]
                      operator [22503-22504] ("=")
                      text [22504-22511] ("lighter")
                      operator [22511-22512] ("=")
                    text [22512-22513] (" ")
                    headline [22513-22523]
                        :level 2:
                      title [22513-22523]
                        operator [22513-22516] ("** ")
                        text [22516-22522] ("Window")
                        newLine [22522-22523]
                      section [22523-22523]
                    headline [22523-22560]
                        :level 3:
                      title [22523-22560]
                        operator [22523-22527] ("*** ")
                        text [22527-22559] ("Получение ширины текущего экрана")
                        newLine [22559-22560]
                      section [22560-22560]
                    verbatim [22560-22582]
                      operator [22560-22561] ("=")
                      text [22561-22581] ("(window-total-width)")
                      operator [22581-22582] ("=")
                    newLine [22582-22583]
                    headline [22583-22807]
                        :level 2:
                      title [22583-22619]
                        operator [22583-22586] ("** ")
                        text [22586-22618] ("Асинхронное исполнение. Process.")
                        newLine [22618-22619]
                      section [22619-22807]
                        indent [22619-22639] ("                    ")
                        text [22639-22651] ("\\"WakatimeUI\\"")
                        newLine [22651-22652]
                        indent [22652-22672] ("                    ")
                        text [22672-22691] ("wakatime-ui--buffer")
                        newLine [22691-22692]
                        indent [22692-22712] ("                    ")
                        text [22712-22734] ("(wakatime-find-binary)")
                        newLine [22734-22735]
                        indent [22735-22755] ("                    ")
                        text [22755-22806] ("(plist-get wakatime-ui--command-args :today-time)))")
                        newLine [22806-22807]
                    text [22807-22839] ("Создание асинхронного процесаа (")
                    link [22839-22950]
                      operator [22839-22840] ("[")
                      linkUrl [22840-22933]
                        operator [22840-22841] ("[")
                        text [22841-22932] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Asynchronous-Processes.html")
                        operator [22932-22933] ("]")
                      linkName [22933-22949]
                        operator [22933-22934] ("[")
                        text [22934-22948] ("подробнее тут)")
                        operator [22948-22949] ("]")
                      operator [22949-22950] ("]")
                    newLine [22950-22951]
                    newLine [22951-22952]
                    srcBlock [22952-23013]
                      blockHeader [22952-22974]
                        keyword [22952-22974]
                          text [22952-22963] ("#+begin_src")
                          text [22963-22974] (" emacs-lisp")
                      newLine [22974-22975]
                      blockBody [22975-23003]
                        text [22975-23003] ("(setq process (start-process")
                      newLine [23003-23004]
                      blockFooter [23004-23013]
                        keyword [23004-23013]
                          text [23004-23013] ("#+end_src")
                    newLine [23013-23014]
                    text [23014-23048] ("Чтение выходных данных из процесса")
                    newLine [23048-23049]
                    newLine [23049-23050]
                    srcBlock [23050-23083]
                      blockHeader [23050-23072]
                        keyword [23050-23072]
                          text [23050-23061] ("#+begin_src")
                          text [23061-23072] (" emacs-lisp")
                      newLine [23072-23073]
                      newLine [23073-23074]
                      blockFooter [23074-23083]
                        keyword [23074-23083]
                          text [23074-23083] ("#+end_src")
                    newLine [23083-23084]
            headline [23084-23713]
                :level 2:
              title [23084-23095]
                operator [23084-23087] ("** ")
                text [23087-23094] ("Keymaps")
                newLine [23094-23095]
              section [23095-23713]
                headline [23095-23713]
                    :level 3:
                  title [23095-23122]
                    operator [23095-23099] ("*** ")
                    text [23099-23121] ("Создание своего keymap")
                    newLine [23121-23122]
                  section [23122-23713]
                    srcBlock [23122-23701]
                      blockHeader [23122-23139]
                        keyword [23122-23139]
                          text [23122-23133] ("#+begin_src")
                          text [23133-23139] (" elisp")
                      newLine [23139-23140]
                      blockBody [23140-23691]
                        text [23140-23691] ("(with-current-buffer \\"*Messages*\\"\\n  (read-only-mode -1)\\n  (erase-buffer))\\n\\n(setq my-mode-map (make-sparse-keymap))\\n(define-key my-mode-map (kbd \\"C-c C-'\\") 'my-mode-cmd1)\\n(define-key my-mode-map (kbd \\"C-c C-b\\") 'my-mode-cmd2)\\n(define-key my-mode-map (kbd \\"C-c C-c\\") 'my-mode-cmd3)\\n(define-key my-mode-map (kbd \\"<mouse-1>\\") 'my-mode-cmd4)\\n;; by convention, major mode's keys should begin with the form C-c C-‹key›\\n\\n;; (dolist (m my-mode-map)\\n;;   (message \\"key: %s\\" m))\\n\\n\\n\\n\\n\\n(map-keymap '(lambda (v g)\\n               (message \\"%s: %s\\" v g)) my-mode-map)")
                      newLine [23691-23692]
                      blockFooter [23692-23701]
                        keyword [23692-23701]
                          text [23692-23701] ("#+end_src")
                    newLine [23701-23702]
                    keyword [23702-23712]
                      text [23702-23712] ("#+RESULTS:")
                    newLine [23712-23713]
            headline [23713-28848]
                :level 2:
                :id  elisp-macros:
              title [23713-23722]
                operator [23713-23716] ("** ")
                text [23716-23721] ("Macro")
                newLine [23721-23722]
              section [23722-28848]
                propertyDrawer [23722-23758]
                  property [23722-23734]
                    text [23722-23734] (":PROPERTIES:")
                  newLine [23734-23735]
                  property [23735-23752]
                    text [23735-23739] (":ID:")
                    text [23739-23752] (" elisp-macros")
                  newLine [23752-23753]
                  property [23753-23758]
                    text [23753-23758] (":END:")
                newLine [23758-23759]
                text [23759-23769] ("Подробнее ")
                link [23769-23853]
                  operator [23769-23770] ("[")
                  linkUrl [23770-23847]
                    operator [23770-23771] ("[")
                    text [23771-23846] ("htest-varps://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html")
                    operator [23846-23847] ("]")
                  linkName [23847-23852]
                    operator [23847-23848] ("[")
                    text [23848-23851] ("тут")
                    operator [23851-23852] ("]")
                  operator [23852-23853] ("]")
                text [23853-23854] (".")
                newLine [23854-23855]
                newLine [23855-23856]
                headline [23856-24030]
                    :level 3:
                  title [23856-23875]
                    operator [23856-23860] ("*** ")
                    text [23860-23874] ("Простой макрос")
                    newLine [23874-23875]
                  section [23875-24030]
                    srcBlock [23875-24012]
                      blockHeader [23875-23897]
                        keyword [23875-23897]
                          text [23875-23886] ("#+begin_src")
                          text [23886-23897] (" emacs-lisp")
                      newLine [23897-23898]
                      blockBody [23898-24002]
                        text [23898-24002] ("(defmacro inc (var)\\n  (list 'setq var (list '1+ var)))\\n\\n(setq test-var 10)\\n(message \\"%s\\" (inc test-var))")
                      newLine [24002-24003]
                      blockFooter [24003-24012]
                        keyword [24003-24012]
                          text [24003-24012] ("#+end_src")
                    newLine [24012-24013]
                    keyword [24013-24024]
                      text [24013-24023] ("#+RESULTS:")
                      text [24023-24024] (" ")
                    newLine [24024-24025]
                    fixedWidth [24025-24029]
                      operator [24025-24027] (": ")
                      text [24027-24029] ("11")
                    newLine [24029-24030]
                headline [24030-24205]
                    :level 3:
                  title [24030-24049]
                    operator [24030-24034] ("*** ")
                    text [24034-24048] ("Изучить макрос")
                    newLine [24048-24049]
                  section [24049-24205]
                    text [24049-24087] ("Macroexpand - показывает вывод макроса")
                    newLine [24087-24088]
                    newLine [24088-24089]
                    srcBlock [24089-24166]
                      blockHeader [24089-24111]
                        keyword [24089-24111]
                          text [24089-24100] ("#+begin_src")
                          text [24100-24111] (" emacs-lisp")
                      newLine [24111-24112]
                      blockBody [24112-24156]
                        text [24112-24156] ("(message \\"%s\\" (macroexpand '(inc test-var)))")
                      newLine [24156-24157]
                      blockFooter [24157-24166]
                        keyword [24157-24166]
                          text [24157-24166] ("#+end_src")
                    newLine [24166-24167]
                    keyword [24167-24178]
                      text [24167-24177] ("#+RESULTS:")
                      text [24177-24178] (" ")
                    newLine [24178-24179]
                    fixedWidth [24179-24204]
                      operator [24179-24181] (": ")
                      text [24181-24204] ("(setq test-var (1+ tt))")
                    newLine [24204-24205]
                headline [24205-24544]
                    :level 3:
                  title [24205-24229]
                    operator [24205-24209] ("*** ")
                    text [24209-24228] ("Цепочка из макросов")
                    newLine [24228-24229]
                  section [24229-24544]
                    text [24229-24311] ("Macroexpand отображает только первый макрос, чтобы вызвать всю цепочку используем ")
                    verbatim [24311-24328]
                      operator [24311-24312] ("=")
                      text [24312-24327] ("macroexpand-all")
                      operator [24327-24328] ("=")
                    newLine [24328-24329]
                    newLine [24329-24330]
                    srcBlock [24330-24489]
                      blockHeader [24330-24352]
                        keyword [24330-24352]
                          text [24330-24341] ("#+begin_src")
                          text [24341-24352] (" emacs-lisp")
                      newLine [24352-24353]
                      blockBody [24353-24479]
                        text [24353-24479] ("(defmacro inc2 (var1 var2)\\n    (list 'progn (list 'inc var1) (list 'inc var2)))\\n\\n\\n(message \\"%s\\" (macroexpand-all '(inc2 r s)))")
                      newLine [24479-24480]
                      blockFooter [24480-24489]
                        keyword [24480-24489]
                          text [24480-24489] ("#+end_src")
                    newLine [24489-24490]
                    keyword [24490-24501]
                      text [24490-24500] ("#+RESULTS:")
                      text [24500-24501] (" ")
                    newLine [24501-24502]
                    fixedWidth [24502-24543]
                      operator [24502-24504] (": ")
                      text [24504-24543] ("(progn (setq r (1+ r)) (setq s (1+ s)))")
                    newLine [24543-24544]
                headline [24544-24752]
                    :level 3:
                  title [24544-24586]
                    operator [24544-24548] ("*** ")
                    text [24548-24585] ("Пример с более сложными конструкциями")
                    newLine [24585-24586]
                  section [24586-24752]
                    srcBlock [24586-24733]
                      blockHeader [24586-24608]
                        keyword [24586-24608]
                          text [24586-24597] ("#+begin_src")
                          text [24597-24608] (" emacs-lisp")
                      newLine [24608-24609]
                      blockBody [24609-24723]
                        text [24609-24723] ("(defmacro t-becomes-nil (var)\\n  \`(if (eq ,var t)\\n       (setq ,var nil)))\\n\\n(message \\"%s\\" (t-becomes-nil test-var))")
                      newLine [24723-24724]
                      blockFooter [24724-24733]
                        keyword [24724-24733]
                          text [24724-24733] ("#+end_src")
                    newLine [24733-24734]
                    keyword [24734-24745]
                      text [24734-24744] ("#+RESULTS:")
                      text [24744-24745] (" ")
                    newLine [24745-24746]
                    fixedWidth [24746-24751]
                      operator [24746-24748] (": ")
                      text [24748-24751] ("nil")
                    newLine [24751-24752]
                headline [24752-25861]
                    :level 3:
                  title [24752-24790]
                    operator [24752-24756] ("*** ")
                    text [24756-24789] ("Динамическое получение переменной")
                    newLine [24789-24790]
                  section [24790-25861]
                    indent [24790-24791] (" ")
                    link [24791-24912]
                      operator [24791-24792] ("[")
                      linkUrl [24792-24894]
                        operator [24792-24793] ("[")
                        text [24793-24893] ("https://stackoverflow.com/questions/24433035/combining-two-variables-into-one-function-name-in-macro")
                        operator [24893-24894] ("]")
                      linkName [24894-24911]
                        operator [24894-24895] ("[")
                        text [24895-24910] ("Подсмотрено тут")
                        operator [24910-24911] ("]")
                      operator [24911-24912] ("]")
                    newLine [24912-24913]
                    indent [24913-24914] (" ")
                    text [24914-24931] ("Чертовая магия 😮")
                    newLine [24931-24932]
                    newLine [24932-24933]
                    srcBlock [24933-25171]
                      blockHeader [24933-24955]
                        keyword [24933-24955]
                          text [24933-24944] ("#+begin_src")
                          text [24944-24955] (" emacs-lisp")
                      newLine [24955-24956]
                      blockBody [24956-25161]
                        text [24956-25161] ("(setq my-custom-variable \\"Hello, amma variable\\")\\n\\n(defmacro get-with-prefix (var-name)\\n  \`(symbol-value (intern (concatenate 'string \\"my-custom\\" \\"-\\" (symbol-name ',var-name)))))\\n\\n(get-with-prefix variable)")
                      newLine [25161-25162]
                      blockFooter [25162-25171]
                        keyword [25162-25171]
                          text [25162-25171] ("#+end_src")
                    newLine [25171-25172]
                    keyword [25172-25183]
                      text [25172-25182] ("#+RESULTS:")
                      text [25182-25183] (" ")
                    newLine [25183-25184]
                    fixedWidth [25184-25206]
                      operator [25184-25186] (": ")
                      text [25186-25206] ("Hello, amma variable")
                    newLine [25206-25207]
                    newLine [25207-25208]
                    text [25208-25374] ("А теперь из plist, если нет - то из глобального скоупа, это еще большая магия. Да, наверное такое не стоит использовать в реальных проектах, но как же руки чешутся 😍")
                    newLine [25374-25375]
                    newLine [25375-25376]
                    srcBlock [25376-25825]
                      blockHeader [25376-25398]
                        keyword [25376-25398]
                          text [25376-25387] ("#+begin_src")
                          text [25387-25398] (" emacs-lisp")
                      newLine [25398-25399]
                      blockBody [25399-25815]
                        text [25399-25815] ("(setq my-custom-variable \\"Hello, amma variable\\")\\n\\n(setq my-plist-with-prop '(:custom-variable nil :test t))\\n\\n(defmacro get-with-prefix (my-plist var-name)\\n  \`(or (plist-get ,my-plist (symbol-value (intern (concatenate 'string \\":\\" (symbol-name ',var-name)))))\\n       (symbol-value (intern (concatenate 'string \\"my\\" \\"-\\" (symbol-name ',var-name))))))\\n\\n(message \\"%s\\" (get-with-prefix my-plist-with-prop custom-variable))")
                      newLine [25815-25816]
                      blockFooter [25816-25825]
                        keyword [25816-25825]
                          text [25816-25825] ("#+end_src")
                    newLine [25825-25826]
                    keyword [25826-25837]
                      text [25826-25836] ("#+RESULTS:")
                      text [25836-25837] (" ")
                    newLine [25837-25838]
                    fixedWidth [25838-25860]
                      operator [25838-25840] (": ")
                      text [25840-25860] ("Hello, amma variable")
                    newLine [25860-25861]
                headline [25861-27010]
                    :level 3:
                  title [25861-25898]
                    operator [25861-25865] ("*** ")
                    text [25865-25887] ("Передача тела (@body) ")
                    tagList [25887-25897]
                      operator [25887-25888] (":")
                      text [25888-25896] ("noexport")
                      operator [25896-25897] (":")
                    newLine [25897-25898]
                  section [25898-27010]
                    text [25898-26052] ("Пожалуй самая впечатлаяющая фича (имхо, без нее смысл в макросах бы отпал). Макрос склеивает результаты выполнения функций (подумал для org-mode самое то)")
                    newLine [26052-26053]
                    newLine [26053-26054]
                    srcBlock [26054-26383]
                      blockHeader [26054-26076]
                        keyword [26054-26076]
                          text [26054-26065] ("#+begin_src")
                          text [26065-26076] (" emacs-lisp")
                      newLine [26076-26077]
                      blockBody [26077-26373]
                        text [26077-26373] ("(setq test-var 0)\\n(defmacro for (var from init to final do &rest body)\\n  \`(let ((,var ,init))\\n     (while (<= ,var ,final)\\n       ,@body\\n       (setq ,var (1+ ,var)))))\\n\\n\\n(for j from 0 to 4 do\\n     (setq test-var (+ test-var j))\\n     (setq test-var (/ test-var 2)))\\n\\n(message \\"HAVA: %s\\" test-var)")
                      newLine [26373-26374]
                      blockFooter [26374-26383]
                        keyword [26374-26383]
                          text [26374-26383] ("#+end_src")
                    newLine [26383-26384]
                    keyword [26384-26395]
                      text [26384-26394] ("#+RESULTS:")
                      text [26394-26395] (" ")
                    newLine [26395-26396]
                    fixedWidth [26396-26405]
                      operator [26396-26398] (": ")
                      text [26398-26405] ("HAVA: 3")
                    newLine [26405-26406]
                    newLine [26406-26407]
                    newLine [26407-26408]
                    headline [26408-27010]
                        :level 4:
                      title [26408-26431]
                        operator [26408-26413] ("**** ")
                        text [26413-26420] ("Failed ")
                        tagList [26420-26430]
                          operator [26420-26421] (":")
                          text [26421-26429] ("noexport")
                          operator [26429-26430] (":")
                        newLine [26430-26431]
                      section [26431-27010]
                        text [26431-26500] ("Пример макроса, чтобы наглядно видеть в орге какая функция что делает")
                        newLine [26500-26501]
                        newLine [26501-26502]
                        srcBlock [26502-26826]
                          blockHeader [26502-26524]
                            keyword [26502-26524]
                              text [26502-26513] ("#+begin_src")
                              text [26513-26524] (" emacs-lisp")
                          newLine [26524-26525]
                          blockBody [26525-26816]
                            text [26525-26816] ("(defmacro pretty-log (&rest body)\\n\\n  (let ((res (concat (make-string 80 ?-) \\"\\n\\")))\\n    (dolist (f body)\\n      (setq res (concat res (format \\"[%s]: %s\\n\\" f (eval f)))))\\n    (message res)))\\n\\n(pretty-log (+ 1 12)\\n            (- 44 22)\\n            (+ (/ 12 2) (* 33 4))\\n            (setq ttt 12))")
                          newLine [26816-26817]
                          blockFooter [26817-26826]
                            keyword [26817-26826]
                              text [26817-26826] ("#+end_src")
                        newLine [26826-26827]
                        keyword [26827-26838]
                          text [26827-26837] ("#+RESULTS:")
                          text [26837-26838] (" ")
                        newLine [26838-26839]
                        fixedWidth [26839-26921]
                          operator [26839-26841] (": ")
                          text [26841-26921] ("--------------------------------------------------------------------------------")
                        newLine [26921-26922]
                        fixedWidth [26922-26938]
                          operator [26922-26924] (": ")
                          text [26924-26938] ("[(+ 1 12)]: 13")
                        newLine [26938-26939]
                        fixedWidth [26939-26956]
                          operator [26939-26941] (": ")
                          text [26941-26956] ("[(- 44 22)]: 22")
                        newLine [26956-26957]
                        fixedWidth [26957-26987]
                          operator [26957-26959] (": ")
                          text [26959-26987] ("[(+ (/ 12 2) (* 33 4))]: 138")
                        newLine [26987-26988]
                        fixedWidth [26988-27009]
                          operator [26988-26990] (": ")
                          text [26990-27009] ("[(setq ttt 12)]: 12")
                        newLine [27009-27010]
                headline [27010-28848]
                    :level 3:
                  title [27010-27098]
                    operator [27010-27014] ("*** ")
                    text [27014-27087] ("Модификация plist через список динамических аргументов как в use-package ")
                    tagList [27087-27097]
                      operator [27087-27088] (":")
                      text [27088-27096] ("noexport")
                      operator [27096-27097] (":")
                    newLine [27097-27098]
                  section [27098-28848]
                    srcBlock [27098-28623]
                      blockHeader [27098-27120]
                        keyword [27098-27120]
                          text [27098-27109] ("#+begin_src")
                          text [27109-27120] (" emacs-lisp")
                      newLine [27120-27121]
                      blockBody [27121-28613]
                        text [27121-28613] ("(setq res \\"\\")\\n(setq test-alist\\n      '((js-mode (:loggers '(\\"hi there\\") :msg-format-template \\"Hi\\" :argument-divider \\"|\\"))\\n        (typescript-mode (:loggers '(\\"another on\\", \\"and me\\") :msg-format-template \\"bee\\"))\\n        ))\\n\\n(defmacro turbo-log-configure (&rest configs)\\n  (let* ((strategy (or (plist-get configs :strategy) 'replace))\\n         (excluded-keys '(:modes :strategy))\\n         (modes (plist-get configs :modes))\\n         current-config)\\n\\n    (dolist (k excluded-keys)\\n      (setq configs (map-delete configs k)))\\n\\n    (dolist (mode modes)\\n      (unless (assoc mode test-alist)\\n        (push \`(,mode nil) test-alist))\\n\\n      (setq current-config (car (cdr-safe (assoc mode test-alist))))\\n\\n      (if (eq strategy 'replace)\\n          (setq current-config configs)\\n\\n        (loop for (k v) on configs by 'cddr do\\n              (if current-config\\n                  (plist-put current-config k v)\\n                (setq current-config \`(,k ,v)))))\\n\\n      (message \\"QQQ: %s\\" configs)\\n      (if (assq mode test-alist)\\n          (setcdr (assq mode test-alist)\\n                  \`(,current-config))\\n        \`(push '(,mode '(,current-config)) ,test-alist))\\n      )))\\n\\n(turbo-log-configure\\n :modes (typescript-mode js2-mode js-mode)\\n ;; :modes (typescript-mode j-mode)\\n ;; :modes (js-mode)\\n :strategy replace\\n\\n :loggers (\\"console.print\\" \\"console.dbg\\")\\n :msg-format-template \\"\\"HELLO WORLD: %s\\"\\")\\n\\n(message \\"-------------------------------------------------------\\")\\n(message \\"%s\\" (pp test-alist))")
                      newLine [28613-28614]
                      blockFooter [28614-28623]
                        keyword [28614-28623]
                          text [28614-28623] ("#+end_src")
                    newLine [28623-28624]
                    keyword [28624-28635]
                      text [28624-28634] ("#+RESULTS:")
                      text [28634-28635] (" ")
                    newLine [28635-28636]
                    fixedWidth [28636-28649]
                      operator [28636-28638] (": ")
                      text [28638-28649] ("((mode nil)")
                    newLine [28649-28650]
                    fixedWidth [28650-28661]
                      operator [28650-28652] (": ")
                      text [28652-28661] (" (js-mode")
                    newLine [28661-28662]
                    fixedWidth [28662-28675]
                      operator [28662-28664] (": ")
                      text [28664-28675] ("  (:loggers")
                    newLine [28675-28676]
                    fixedWidth [28676-28694]
                      operator [28676-28678] (": ")
                      text [28678-28694] ("   '(\\"hi there\\")")
                    newLine [28694-28695]
                    fixedWidth [28695-28727]
                      operator [28695-28697] (": ")
                      text [28697-28727] ("   :msg-format-template \\"Hi\\"))")
                    newLine [28727-28728]
                    fixedWidth [28728-28747]
                      operator [28728-28730] (": ")
                      text [28730-28747] (" (typescript-mode")
                    newLine [28747-28748]
                    fixedWidth [28748-28761]
                      operator [28748-28750] (": ")
                      text [28750-28761] ("  (:loggers")
                    newLine [28761-28762]
                    fixedWidth [28762-28798]
                      operator [28762-28764] (": ")
                      text [28764-28798] ("   (\\"console.print\\" \\"console.dbg\\")")
                    newLine [28798-28799]
                    fixedWidth [28799-28847]
                      operator [28799-28801] (": ")
                      text [28801-28847] ("   :msg-format-template \\"\\"HELLO WORLD: %s\\"\\")))")
                    newLine [28847-28848]
        headline [28848-29280]
            :level 1:
          title [28848-28873]
            operator [28848-28850] ("* ")
            text [28850-28872] ("Создание своего пакета")
            newLine [28872-28873]
          section [28873-29280]
            headline [28873-29035]
                :level 2:
              title [28873-28903]
                operator [28873-28876] ("** ")
                text [28876-28902] ("Проверка ошибок компиляции")
                newLine [28902-28903]
              section [28903-29035]
                srcBlock [28903-29034]
                  blockHeader [28903-28919]
                    keyword [28903-28919]
                      text [28903-28914] ("#+begin_src")
                      text [28914-28919] (" bash")
                  newLine [28919-28920]
                  blockBody [28920-29024]
                    text [28920-29024] ("emacs -Q --batch     --eval '(setq byte-compile-error-on-warn t)'     -f batch-byte-compile turbo-log.el")
                  newLine [29024-29025]
                  blockFooter [29025-29034]
                    keyword [29025-29034]
                      text [29025-29034] ("#+end_src")
                newLine [29034-29035]
            headline [29035-29098]
                :level 2:
              title [29035-29049]
                operator [29035-29038] ("** ")
                text [29038-29048] ("Contribute")
                newLine [29048-29049]
              section [29049-29098]
                link [29049-29097]
                  operator [29049-29050] ("[")
                  linkUrl [29050-29096]
                    operator [29050-29051] ("[")
                    text [29051-29095] ("htest-varps://github.com/leotaku/elisp-check")
                    operator [29095-29096] ("]")
                  operator [29096-29097] ("]")
                newLine [29097-29098]
            headline [29098-29280]
                :level 2:
              title [29098-29104]
                operator [29098-29101] ("** ")
                text [29101-29103] ("CI")
                newLine [29103-29104]
              section [29104-29280]
                link [29104-29213]
                  operator [29104-29105] ("[")
                  linkUrl [29105-29189]
                    operator [29105-29106] ("[")
                    text [29106-29188] ("htest-varps://github.com/a13/reverse-im.el/blob/master/.github/workflows/check.yml")
                    operator [29188-29189] ("]")
                  linkName [29189-29212]
                    operator [29189-29190] ("[")
                    text [29190-29211] ("Пример github actions")
                    operator [29211-29212] ("]")
                  operator [29212-29213] ("]")
                newLine [29213-29214]
                link [29214-29279]
                  operator [29214-29215] ("[")
                  linkUrl [29215-29261]
                    operator [29215-29216] ("[")
                    text [29216-29260] ("htest-varps://github.com/leotaku/elisp-check")
                    operator [29260-29261] ("]")
                  linkName [29261-29278]
                    operator [29261-29262] ("[")
                    text [29262-29277] ("Про elisp check")
                    operator [29277-29278] ("]")
                  operator [29278-29279] ("]")
                newLine [29279-29280]
        headline [29280-29647]
            :level 1:
          title [29280-29288]
            operator [29280-29282] ("* ")
            text [29282-29287] ("Тесты")
            newLine [29287-29288]
          section [29288-29647]
            text [29288-29417] ("Тесты пишутся весьма просто. От части потому что не нужно мокать кучу зависимостей. Функция в большинстве случаев самодостаточна.")
            newLine [29417-29418]
            newLine [29418-29419]
            srcBlock [29419-29510]
              blockHeader [29419-29441]
                keyword [29419-29441]
                  text [29419-29430] ("#+begin_src")
                  text [29430-29441] (" emacs-lisp")
              newLine [29441-29442]
              blockBody [29442-29500]
                text [29442-29500] ("(ert-deftest my-first-test ()\\n  (should (= (+ 10 10) 20)))")
              newLine [29500-29501]
              blockFooter [29501-29510]
                keyword [29501-29510]
                  text [29501-29510] ("#+end_src")
            newLine [29510-29511]
            text [29511-29518] ("Запуск.")
            newLine [29518-29519]
            newLine [29519-29520]
            srcBlock [29520-29623]
              blockHeader [29520-29536]
                keyword [29520-29536]
                  text [29520-29531] ("#+begin_src")
                  text [29531-29536] (" bash")
              newLine [29536-29537]
              blockBody [29537-29613]
                text [29537-29613] ("emacs -batch -l ert -l package.el -l test.el -f ert-run-tests-batch-and-exit")
              newLine [29613-29614]
              blockFooter [29614-29623]
                keyword [29614-29623]
                  text [29614-29623] ("#+end_src")
            newLine [29623-29624]
            undefined [29624-29647]
              blockHeader [29624-29640]
                keyword [29624-29640]
                  text [29624-29640] ("#+BEGIN_{HIDDEN}")
              newLine [29640-29641]
              blockBody [29641-29641]
              blockFooter [29641-29647]
                keyword [29641-29647]
                  text [29641-29647] ("#+END_")
        headline [29647-29743]
            :level 1:
          title [29641-29668]
            operator [29641-29643] ("* ")
            text [29643-29667] ("Статический анализ типов")
            newLine [29667-29668]
          section [29668-29737]
            link [29668-29736]
              operator [29668-29669] ("[")
              linkUrl [29669-29705]
                operator [29669-29670] ("[")
                text [29670-29704] ("https://github.com/emacs-elsa/Elsa")
                operator [29704-29705] ("]")
              linkName [29705-29735]
                operator [29705-29706] ("[")
                text [29706-29734] ("Его нет. Зато есть аннотации")
                operator [29734-29735] ("]")
              operator [29735-29736] ("]")
            newLine [29736-29737]
        headline [29743-32125]
            :level 1:
          title [29737-29759]
            operator [29737-29739] ("* ")
            text [29739-29748] ("Временно ")
            tagList [29748-29758]
              operator [29748-29749] (":")
              text [29749-29757] ("noexport")
              operator [29757-29758] (":")
            newLine [29758-29759]
          section [29759-32119]
            srcBlock [29759-29807]
              blockHeader [29759-29781]
                keyword [29759-29781]
                  text [29759-29770] ("#+begin_src")
                  text [29770-29781] (" emacs-lisp")
              newLine [29781-29782]
              blockBody [29782-29797]
                text [29782-29797] ("(message \\"\\"\\\\\\"\\")")
              newLine [29797-29798]
              blockFooter [29798-29807]
                keyword [29798-29807]
                  text [29798-29807] ("#+end_src")
            newLine [29807-29808]
            srcBlock [29808-29943]
              blockHeader [29808-29830]
                keyword [29808-29830]
                  text [29808-29819] ("#+begin_src")
                  text [29819-29830] (" emacs-lisp")
              newLine [29830-29831]
              blockBody [29831-29933]
                text [29831-29933] ("(message \\"%s\\" (string-match \\"{\\\\|);?$\\" \\"public replaceNonPrintableCharacters(text: string): string {\\"))")
              newLine [29937-29938]
              blockFooter [29938-29947]
                keyword [29938-29947]
                  text [29934-29943] ("#+end_src")
            newLine [29943-29944]
            keyword [29944-29955]
              text [29944-29954] ("#+RESULTS:")
              text [29954-29955] (" ")
            newLine [29955-29956]
            fixedWidth [29956-29960]
              operator [29956-29958] (": ")
              text [29958-29960] ("59")
            newLine [29960-29961]
            newLine [29961-29962]
            newLine [29962-29963]
            srcBlock [29963-30144]
              blockHeader [29963-29985]
                keyword [29963-29985]
                  text [29963-29974] ("#+begin_src")
                  text [29974-29985] (" emacs-lisp")
              newLine [29985-29986]
              blockBody [29986-30134]
                text [29986-30134] ("(setq turbo-log--ecmascript-final-symbols '(?; ?)))\\n(while (or (not (eobp)) (member ?) '(?; ?))))\\n                 (setq current-char char-after))))")
              newLine [30134-30135]
              blockFooter [30135-30144]
                keyword [30135-30144]
                  text [30135-30144] ("#+end_src")
            newLine [30144-30145]
            srcBlock [30145-31166]
              blockHeader [30145-30167]
                keyword [30145-30167]
                  text [30145-30156] ("#+begin_src")
                  text [30156-30167] (" emacs-lisp")
              newLine [30167-30168]
              blockBody [30168-31156]
                text [30168-31156] ("(setq quicktype-mode-configs '((\\"go\\" go-mode \\"\\")\\n                               (\\"ts\\" typescript-mode \\"\\")\\n                               (\\"js\\" js2-mode \\"\\")\\n                               (\\"rs\\" rust-mode \\"\\")\\n                               (\\"c++\\" c++(\\"c++\\" c++-mode \\"\\")\\n                               (\\"javascript-prop-types\\" js2-mode \\"\\")\\n                               (\\"flow\\" flow-js2-mode \\"\\")\\n                               (\\"swift\\" swift-mode \\"\\")\\n                               (\\"kotlin\\" kotlin-mode \\"\\")\\n                               (\\"elm\\" elm-mode \\"\\")\\n                               (\\"ruby\\" ruby-mode \\"\\")\\n                               (\\"dart\\" dart-mode \\"\\")\\n                               (\\"py\\" python-mode \\"--python-version 3.7\\")\\n                               (\\"haskell\\" haskell-mode \\"\\")))\\n\\n;; (message \\"%s\\" quicktype-mode-configs)\\n(message \\"%s\\" (cl-rassoc 'go-mode quicktype-mode-configs :test #'member))\\n;; (message \\"%s\\" (cl-rassoc \\"Red Pine\\" needles-per-cluster :test #'member))")
              newLine [31156-31157]
              blockFooter [31157-31166]
                keyword [31157-31166]
                  text [31157-31166] ("#+end_src")
            newLine [31166-31167]
            keyword [31167-31178]
              text [31167-31177] ("#+RESULTS:")
              text [31177-31178] (" ")
            newLine [31178-31179]
            fixedWidth [31179-31194]
              operator [31179-31181] (": ")
              text [31181-31194] ("(go go-mode )")
            newLine [31194-31195]
            newLine [31195-31196]
            newLine [31196-31197]
            srcBlock [31197-31420]
              blockHeader [31197-31219]
                keyword [31197-31219]
                  text [31197-31208] ("#+begin_src")
                  text [31208-31219] (" emacs-lisp")
              newLine [31219-31220]
              blockBody [31220-31410]
                text [31220-31410] ("(setq needles-per-cluster\\n      '((2 \\"Austrian Pine\\" \\"Red Pine\\")\\n        (3 \\"Pitch Pine\\")\\n        (5 \\"White Pine\\")))\\n\\n(message \\"%s\\" (cl-rassoc \\"Red Pine\\" needles-per-cluster :test #'member))")
              newLine [31410-31411]
              blockFooter [31411-31420]
                keyword [31411-31420]
                  text [31411-31420] ("#+end_src")
            newLine [31420-31421]
            keyword [31421-31432]
              text [31421-31431] ("#+RESULTS:")
              text [31431-31432] (" ")
            newLine [31432-31433]
            fixedWidth [31433-31461]
              operator [31433-31435] (": ")
              text [31435-31461] ("(2 Austrian Pine Red Pine)")
            newLine [31461-31462]
            newLine [31462-31463]
            newLine [31463-31464]
            srcBlock [31464-31600]
              blockHeader [31464-31486]
                keyword [31464-31486]
                  text [31464-31475] ("#+begin_src")
                  text [31475-31486] (" emacs-lisp")
              newLine [31486-31487]
              blockBody [31487-31590]
                text [31487-31590] ("(message \\"%s\\" (string-match \\"\\\\({\\\\|;$\\\\)\\\\|\\\\(const [\\\\w\\\\[:digit]]+ = [\\\\d[:digit:]]+$\\\\)\\" \\"  const foo = 1\\"))")
              newLine [31590-31591]
              blockFooter [31591-31600]
                keyword [31591-31600]
                  text [31591-31600] ("#+end_src")
            newLine [31600-31601]
            keyword [31601-31612]
              text [31601-31611] ("#+RESULTS:")
              text [31611-31612] (" ")
            newLine [31612-31613]
            fixedWidth [31613-31618]
              operator [31613-31615] (": ")
              text [31615-31618] ("nil")
            newLine [31618-31619]
            text [31619-31627] ("{HIDDEN}")
            newLine [31627-31628]
            newLine [31628-31629]
            srcBlock [31629-31729]
              blockHeader [31629-31651]
                keyword [31629-31651]
                  text [31629-31640] ("#+begin_src")
                  text [31640-31651] (" emacs-lisp")
              newLine [31651-31652]
              blockBody [31652-31719]
                text [31652-31719] ("(setq v (dolist (i '(1 2 3 4))\\n                i))\\n(message \\"%s\\" v)")
              newLine [31719-31720]
              blockFooter [31720-31729]
                keyword [31720-31729]
                  text [31720-31729] ("#+end_src")
            newLine [31729-31730]
            keyword [31730-31741]
              text [31730-31740] ("#+RESULTS:")
              text [31740-31741] (" ")
            newLine [31741-31742]
            fixedWidth [31742-31747]
              operator [31742-31744] (": ")
              text [31744-31747] ("nil")
            newLine [31747-31748]
            newLine [31748-31749]
            newLine [31749-31750]
            headline [31750-32119]
                :level 2:
              title [31750-31764]
                operator [31750-31753] ("** ")
                text [31753-31763] ("Check json")
                newLine [31763-31764]
              section [31764-32119]
                srcBlock [31764-32108]
                  blockHeader [31764-31786]
                    keyword [31764-31786]
                      text [31764-31775] ("#+begin_src")
                      text [31775-31786] (" emacs-lisp")
                  newLine [31786-31787]
                  blockBody [31787-32098]
                    text [31787-32098] ("  (let* ((json-object-type 'plist)\\n         (json-array-type 'list)\\n         (json-key-type 'string)\\n         (json (json-read-file web-roam-configuration-file-path))\\n         (name-to-config (make-hash-table :test 'equal))\\n         (server-names '()))\\n    (dolist (config json)\\n      (message \\"%s\\" config))\\n  )")
                  newLine [32098-32099]
                  blockFooter [32099-32108]
                    keyword [32099-32108]
                      text [32099-32108] ("#+end_src")
                newLine [32108-32109]
                keyword [32109-32119]
                  text [32109-32119] ("#+RESULTS:")
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
