# Knowledge Compilation
Final project for subject Probabilistic Logic Programming implementing knowledge compilation using BDDs in prolog.

## Logic flow
1. Ground program
2. Break cycles
3. Convert to logic formula
4. Construct BDD
    1. Construct classic BDD
    
        Recursive Shannon Expansion
    2. Convert to OBDD

        BDD is already an OBDD with random variable ordering, and determining best ordering is NP-hard so this step can be skipped. Some heuristics can be used as well
    3. Reduce to to ROBDD
        - Elimination rule 

            If v is an inner node and both of its children
            points to the same node w, then remove v and
            redirect all incoming edges to w.
        - Isomorphism rule 

            If the nodes v and w are terminals and if they
            have the same value, then remove node v and
            redirect all incoming edges to node w. If v and w are
            inner nodes, they are labeled by the same boolean
            variable and their children are the same, then remove
            node v and redirect all incoming edges to w.
5. Traverse BDD calculating the result
    ```js
    function PROB(node) {
        if node is a terminal {
            return terminal
        } else {
            if Table[node] != null {
                return Table[node]
            } else {
                let p0 = PROB(child0(node))
                let p1 = PROB(child1(node))
                // let π be the probability of being true of var(node)1
                let Res = p1 * π + p0 * ( 1 - π)
                Table[node] = Res
                return Res
            }
        }
    }
    ```

### Usefull links
- http://cc.ee.ntu.edu.tw/~jhjiang/instruction/courses/spring11-eda/lec05-2_2p.pdf
- https://people.eecs.berkeley.edu/~sseshia/219c/lectures/BinaryDecisionDiagrams.pdf
- https://www.uio.no/studier/emner/matnat/ifi/INF5140/v15/slides/obdd.pdf