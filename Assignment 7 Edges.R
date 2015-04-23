users = read.csv("users.csv")
edges = read.csv("edges.csv")

g = graph.data.frame(edges, FALSE,users)

V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray" 

plot(g, vertex.label=NA)