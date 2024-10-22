(ns zouks.main-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.string :as str]
   [zouks.main :as sut]))

;; TODOs
;; - report location of error on invalid json input
;; - An unclosed string should return an error
;; - support negative numbers
;; - support floating point numbers
;; - support floating point numbers of the form 6.02e-23
;; -
;;    { "key:value }

;; For the seeming inconsistency in bracket names see here:
;; https://en.wikipedia.org/wiki/Bracket

(defn is-first-lexeme
  [expected s]
  (is (= expected
         (-> s
             sut/lex
             first
             :token-type))))

(defn has-token-types
  [expected-types s]
  (is (= expected-types (map :token-type (sut/lex s)))))

(deftest -main-test
  (testing "The main method"
    (is (= "Hello World!\n" (with-out-str (sut/-main))))))

(deftest lexer-test
  (testing "A lexer should"
    (testing "generate EOF token for empty string"
      (is-first-lexeme :eof ""))
    (testing
     "generate an :left-brace token when \\{ is encountered"
      (is-first-lexeme :left-brace "{"))
    (testing
     "generate a :right-brace token when \\} is encountered"
      (is-first-lexeme :right-brace "}"))
    (testing "generate a sequence of tokens"
      (has-token-types [:left-brace :right-brace :eof] "{}"))
    (testing "ignore whitespace"
      (has-token-types [:left-brace :right-brace :eof] "{     }"))
    (testing "generate a string token when a string is encountered"
      (is (= {:token-type :string :value "This is a string"}
             (first (sut/lex "\"This is a string\"")))))
    (testing "generate a :colon token when `:` is encountered"
      (is-first-lexeme :colon ":"))
    (testing "be able to lex a key value pair"
      (has-token-types [:left-brace :string :colon :string :right-brace :eof]
                       "{\"key\": \"value\"}"))
    (testing "be able to handle multiple key value pairs"
      (has-token-types
       [:left-brace :string :colon :string :comma :string :colon
        :string :right-brace :eof]
       "{
          \"key\": \"value\",
          \"key2\": \"value2\"
        }"))
    (testing "generate a boolean token when `true` is encountered"
      (is (= {:token-type :boolean :value true}
             (first (sut/lex "true")))))
    (testing "generate an error if true is not followed by a non-character"
      (is (some? (:error (sut/lex "truely")))))
    (testing "generate a boolean token when `false` is encountered"
      (is (= {:token-type :boolean :value false}
             (first (sut/lex "false")))))
    (testing "generate an error if false is not followed by a non-character"
      (is (some? (:error (sut/lex "falsey")))))
    (testing "generate a nil token if `null` is encountered"
      (is (= {:token-type :null :value nil}
             (first (sut/lex "null")))))
    (testing "generate a numeric token when a number is encountered"
      (is (= {:token-type :number :value 125}
             (first (sut/lex "125")))))
    (testing "generate a left square bracket if [ is encountered"
      (is (= {:token-type :left-square-bracket :value "["}
             (first (sut/lex "[")))))
    (testing "generate a right square bracket if ] encountered"
      (is (= {:token-type :right-square-bracket :value "]"}
             (first (sut/lex "]")))))))

(deftest parse-test
  (testing "An empty json should parse to an empty map"
    (is (= {} (sut/parse "{}"))))
  (testing "A json with a mapping should parse to a clojure map with  mapping"
    (is (= "value"
           (-> "{\"key\": \"value\"}"
               sut/parse
               (get "key")))))
  (testing "A json mapping with multiple key value pairs"
    (let
     [json
      (sut/parse
       "{
           \"key\": \"value\",
           \"key2\": \"value2\"
         }")]
      (testing "parses the first mapping"
        (is (= "value" (get json "key"))))
      (testing "parses the second mapping"
        (is (= "value2" (get json "key2"))))))
  (testing "A valid json mapping cannot start with a comma"
    (let
     [json
      (sut/parse
       "{ ,
           \"key\": \"value\",
           \"key2\": \"value2\"
         }")]
      (is (some? (:error json)))))
  (testing "A json mapping mapped to a true value"
    (let [json (sut/parse "{ \"key\": true }")]
      (is (true? (get json "key")))))
  (testing "A json mapping mapped to a false value"
    (let [json (sut/parse "{ \"key\": false }")]
      (is (false? (get json "key")))))
  (testing "A json mapping mappeed to a `null` value"
    (let [json (sut/parse "{ \"key\": null }")]
      (is (nil? (get json "key")))
      (is (some #{"key"} (keys json)))))
  (testing "A json mapping mapped to an integer value"
    (let [json (sut/parse "{ \"key\": 42 }")]
      (is (= 42 (get json "key")))))
  (testing "A lexer error results in a parser error"
    (is (str/includes? (:error (sut/parse "False")) "Unexpected token")))
  (testing "A json mapping mapped to an empty list"
    (let [json (sut/parse "{ \"key\": [] }")]
      (is (= [] (get json "key")))))
  (testing "A json mapping mapped to a singleton list of number"
    (let [json (sut/parse "{ \"key\": [42] }")]
      (is (= [42] (get json "key")))))
  (testing "A json mapping mapped to a singleton list of boolean"
    (let [json (sut/parse "{ \"key\": [false] }")]
      (is (= [false] (get json "key")))))
  (testing "A json mapping mapped to a singleton list of null"
    (let [json (sut/parse "{ \"key\": [null] }")]
      (is (= [nil] (get json "key")))))
  (testing "A json mapping mapped to a singleton list of String"
    (let [json (sut/parse "{ \"key\": [\"value\"] }")]
      (is (= ["value"] (get json "key")))))
  (testing "A json mapping mapped to a mixed list"
    (let [json (sut/parse "{ \"key\": [\"value\", 42, true, null] }")]
      (is (= ["value" 42 true nil] (get json "key")))))
  (testing "A valid lsist cannot start with a comma"
    (let [json (sut/parse "{ \"key\": [, \"value\", 42, true, null] }")]
      (is (some? (:error json))))))

(comment
  ;; This parses correctly
  (sut/parse
   "{
      \"key1\": true,
      \"key2\": false,
      \"key3\": null,
      \"key4\": \"value\",
      \"key5\": 101
   }")

  (sut/lex "False")

  ;; This fails on lexer phase
  (sut/parse
   "{
      \"key1\": true,
      \"key2\": False,
      \"key3\": null,
      \"key4\": \"value\",
      \"key5\": 101
    }"))

(deftest valid-json?-test
  (testing "A minimal valid json string is `{}`"
    (is (sut/valid-json? "{}")))
  (testing "An empty string is not valid json"
    (is (not (sut/valid-json? ""))))
  (testing "A string with only spaces is not valid json"
    (is (not (sut/valid-json? "     "))))
  (testing "An opening brace without a closing brace is not valid json"
    (is (not (sut/valid-json? "{"))))
  (testing "A closing brace without an opning brace is not valid json"
    (is (not (sut/valid-json? "}"))))
  (testing "A valid json string with a single key/value pair"
    (is (sut/valid-json? "{\"key\": \"value\"}")))
  (testing "A key without a value is not a valid kv pair"
    (is (not (sut/valid-json? "{\"key\"}"))))
  (testing "A key value pair without a value is not valid json"
    (is (not (sut/valid-json? "{\"key\":}"))))
  (testing "A comma not followed by another key/value pair is not valid json"
    (is (not (sut/valid-json? "{\"key\": \"value\",}")))))

(deftest exit-code-test
  (testing "A valid json string should exit with code 0"
    (is (= 0 (:exit (sut/parse-and-exit "{}")))))
  (testing "An invalid json string should exit with code 1"
    (is (= 1 (:exit (sut/parse-and-exit ""))))))
