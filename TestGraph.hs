import Graph as G 

graph = G.addEdge (1,2) (G.addEdge (0, 1) (G.addVertex 2 (G.addVertex 1 G.initGraph)))

main = do
    print(graph)
    print(neighbors 0 graph)
    print(reachable 0 graph)
    print(neighbors 2 graph)
    print(neighbors 0 (delEdge (0, 1) graph))
    print(path 0 1 graph)
    