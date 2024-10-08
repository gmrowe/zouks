(ns zouks.main-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [zouks.main :as sut]))

;; TODOs
;; - report location of error on invalid json input
;; - ignore whitespace

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
  (testing "A lexer should generate EOF token for empty string"
    (is-first-lexeme :eof ""))
  (testing
    "A lexer should generate an :left-brace token when \\{ is encountered"
    (is-first-lexeme :left-brace "{"))
  (testing
    "A lexer should generate a :right-brace token when \\} is encountered"
    (is-first-lexeme :right-brace "}"))
  (testing "A lexer should generate a sequence of tokens"
    (has-token-types [:left-brace :right-brace :eof] "{}"))
  (testing "A lexer should ignore whitespace"
    (has-token-types [:left-brace :right-brace :eof] "{     }"))
  (testing "A lexer should generate a string token when a string is encountered"
    (is (= {:token-type :string :value "This is a string"}
           (first (sut/lex "\"This is a string\"")))))
  (testing "A lexer should generate a :colon token when \\: is encountered"
    (is-first-lexeme :colon ":"))
  (testing "A lexer should be able to lex a key value pair"
    (has-token-types [:left-brace :string :colon :string :right-brace :eof]
                     "{\"key\": \"value\"}"))
  (testing "A lexer should be able to handle multiple key value pairs"
    (has-token-types [:left-brace :string :colon :string :comma :string :colon
                      :string :right-brace :eof]
                     "{
  \"key\": \"value\",
  \"key2\": \"value2\"
}")))

(deftest parse-test
  (testing "An empty json should parse to an empty map"
    (is (= {} (sut/parse "{}"))))
  (testing "A json with a mapping should parse to a clojure map with mapping"
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
        (is (= "value2" (get json "key2")))))))

(deftest valid-json?-test
  (testing "A minimal valid json string is `{}`"
    (is (sut/valid-json? "{}")))
  (testing "An empty string is not valid json"
    (is (not (sut/valid-json? ""))))
  (testing "An openting brace without a closing brace is not valid json"
    (is (not (sut/valid-json? "{"))))
  (testing "A valid json string with a single key/value pair"
    (is (sut/valid-json? "{\"key\": \"value\"}")))
  (testing "A key without a value is not a valid kv pair"
    (is (not (sut/valid-json? "{\"key\"}"))))
  (testing "A key value pair without a value is not valid json"
    (is (not (sut/valid-json? "{\"key\"}:")))))

(deftest exit-code-test
  (testing "A valid json string should exit with code 0"
    (is (= 0 (:exit (sut/parse-and-exit "{}")))))
  (testing "An invalid json string should exit with code 1"
    (is (= 1 (:exit (sut/parse-and-exit ""))))))
