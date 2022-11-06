# Visnettwork test


library(visNetwork)

nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(1,3))
visNetwork(nodes, edges, width = "100%")

visNetwork(dot = 'dinetwork {1 -> 1 -> 2; 2 -> 3; 2 -- 4; 2 -> 1 }', 
           width = "100%")

visNetwork(dot = '{
    C_0 -- H_0 [label="link"]
    C_0 -- H_1 [style=dashed,color=red];
    C_0 -- H_2 [type=s];
    C_0 -- C_1 [type=s];
    C_1 -- H_3 [type=s];
    C_1 -- H_4 [type=s];
    C_1 -- H_5 [type=s];
}', width = "100%") 


visNetwork(dot = 'digraph {
  graph [overlap = true, fontsize = 10]
  node [shape = box,
        fontname = Helvetica]
 // A; B; C; D; E; F
  A; B; D; F
  subgraph {rank = same; C; E; B }
  node [shape = circle,
        fixedsize = true,
        width = 0.9] // sets as circles
  1; 2; 3; 4; 5; 6; 7; 8
  A->1 B->2 B->3 B->4 C->A
  1->D E->A 2->4 1->5 1->F
  E->6 4->6 5->7 6->7 3->8
}')

visNetwork(dot = '{A->{B C}}')
