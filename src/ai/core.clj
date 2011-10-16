(ns ai.core)

(def node (create-struct :elem :node))
(def tree '(1 
               ((2 ((5) (6))) (3 ((7 ((11) (12) )) (8 ((13))))) (4 ((9) (10))))))
(def tree2 '(1 ((2 ((3) (4))) (5 ( (6) (7))) (8 ( (9) (10))  ))))

(defn choose-candidate-bfs [nodes-list]
  (first nodes-list))

(defn choose-candidate-dfs [nodes-list]
  (last nodes-list))

(defn grab-elem [node]
  (first node))

(defn child-nodes [node] 
  (second node))

(defn bfs [node target]
  (loop 
    [explored (list (grab-elem node))
     frontier (child-nodes node)]
    (let [possible-target (grab-elem (choose-candidate-bfs frontier))] 
      (cond 
        (empty? frontier) explored
        (= possible-target target) (concat explored (list possible-target))
        :else
          (do
          (recur (concat explored (list possible-target)) 
                 (concat (rest frontier) (child-nodes (choose-candidate-bfs frontier)))))))))

(defn dfs-rigth [node target]
  (loop 
    [explored (list (grab-elem node))
     frontier (child-nodes node)]
    (let [possible-target (grab-elem (choose-candidate-dfs frontier))] 
      (cond 
        (empty? frontier) explored
        (= possible-target target) (concat explored (list possible-target))
        :else
          (do
          (println frontier)
          (recur (concat explored (list possible-target)) 
                 (concat (butlast frontier) (child-nodes (choose-candidate-dfs frontier)))))))))
(defn new-frontier-dfs [old-frontier new-candidate]
  (let [childs (child-nodes new-candidate)]
    (cond 
      (empty? childs) (rest old-frontier)
      :else (conj (rest old-frontier) childs))))

(defn dfs-left [node target]
  (loop 
    [explored (list (grab-elem node))
     frontier (child-nodes node)]
    (let [possible-target (grab-elem (choose-candidate-bfs frontier))] 
      (cond 
        (empty? frontier) explored
        (= possible-target target) (concat explored (list possible-target))
        :else
          (do
          (println frontier)
          (recur (concat explored (list possible-target)) 
                 (new-frontier-dfs frontier (choose-candidate-bfs frontier))))))))

