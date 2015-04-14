(ns markov-hitchhiker.generator-test
  (:require [clojure.test :refer :all]
            [markov-hitchhiker.generator :refer :all]))

(deftest test-word-chain
  (testing "it produces a chain of the possible two step transitions between suffixes and prefixes"
    (let [example '(("A" "certain" "king")
                    ("certain" "king" "had")
                    ("king" "had" "a")
                    ("beautiful" "garden" "and"))]
      (is (= {["A" "certain"] #{"king"}
              ["beautiful" "garden"] #{"and"}
              ["certain" "king"] #{"had"}
              ["king" "had"] #{"a"}}
             (word-chain example))))))

(deftest test-text->word-chain
  (testing "string with spaces and newlines"
    (let [example "A certain king had a beautiful garden and"]
     (is (= {["A" "certain"] #{"king"}
             ["a" "beautiful"] #{"garden"}
             ["and" nil] #{}
             ["beautiful" "garden"] #{"and"}
             ["certain" "king"] #{"had"}
             ["garden" "and"] #{}
             ["had" "a"] #{"beautiful"}
             ["king" "had"] #{"a"}}
            (text->word-chain example))))))

(deftest test-walk-chain
  (let [chain {["A" "certain"] #{"king"}
               ["and" nil] #{}
               ["beautiful" "garden"] #{"and"}
               ["certain" "king"] #{"had"}
               ["garden" "and"] #{}
               ["had" "a"] #{"beautiful"}
               ["king" "had"] #{"a"}
               ["a" "beautiful"] #{"garden" "king"}
               ["beautiful" "king"] #{"had"}}]
    (testing "dead end"
      (let [prefix ["beautiful" "garden"]]
        (is (= ["beautiful" "garden" "and"]
               (walk-chain prefix chain prefix)))))
    (testing "multiple choices"
      (with-redefs [shuffle (fn [c] c)]
        (let [prefix ["a" "beautiful"]]
          (is (= ["a" "beautiful" "garden" "and"]
                 (walk-chain prefix chain prefix))))))
      (with-redefs [shuffle (fn [c] (reverse c))]
        (let [prefix ["king" "had"]]
          (is (> 140
                 (count (apply str (walk-chain prefix chain prefix)))))
          (is (= ["king" "had" "a" "beautiful" "king" "had" "a" "beautiful"]
                 (take 8 (walk-chain prefix chain prefix))))))))

(deftest test-generate-text
  (with-redefs [shuffle (fn [c] c)]
    (let [chain {["A" "certain"] #{"king"}
                 ["and" nil] #{}
                 ["beautiful" "garden"] #{"and"}
                 ["certain" "king"] #{"had"}
                 ["garden" "and"] #{}
                 ["had" "a"] #{"beautiful"}
                 ["king" "had"] #{"a"}
                 ["a" "beautiful"] #{"garden" "king"}
                 ["beautiful" "king"] #{"had"}}]
      (is (= "beautiful garden and" (generate-text "beautiful garden" chain)))
      (is (= "beautiful king had a beautiful garden and" (generate-text "beautiful king" chain)))))) 

(deftest test-end-at-last-punctuation
  (testing "Ends at the last puncuation"
    (is (= "In a tree so happy are we."
           (end-at-last-punctuation "In a tree so happy are we. So that")))
    (testing "Replaces ending comma with a period"
    (is (= "In a tree so happy are we."
           (end-at-last-punctuation "In a tree so happy are we, So that"))))
    (testing "If there are no previous puncations, just leave it alone and add one at the end"
      (is ( = "In the light of the blue moon."
              (end-at-last-punctuation  "In the light of the blue moon there"))))
    (testing "works with multiple punctuation"
      (is ( = "In the light of the blue moon.  We danced merrily."
              (end-at-last-punctuation  "In the light of the blue moon.  We danced merrily.  Be"))))))
