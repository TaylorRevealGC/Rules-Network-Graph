# Rule graphs
library(igraph)
library(RColorBrewer)

# Rule A: If PAYROLL <= 0 THEN RCPTOT = NaN
# Rule B: If EMP <= 0 THEN PAYROLL = 0
# Rule C: If PAYROLL > 0 & EMP > 0 & RCPTOT == NaN THEN RCPTOT = {param1}*PAYROLL + {param2}*EMP
# Rule D: If RCPTOT < 100 THEN RCPTOT_FLAG = 'A' ELSE IF RCPTOT >= 100 & RCPTOT <= 500 THEN RCPTOT_FLAG = 'B' ELSE RCPTOT_FLAG = 'C'
# Rule E: If RCPTOT_FLAG == 'C' THEN REVIEW = 1
# Rule F: If EMP > 1000 THEN EMPTYPE = 'L'
# Rule G: If BESTADMIN == NaN THEN ADMIN_FLAG = 'I'
# Rule H: If ADMIN_FLAG == 'I' THEN PARAM3 = 0.8
# Rule I: If PARAM3 > 0.5 THEN BESTADMIN = NaN
# Rule J: If REVIEW == 1 THEN ASSIGN = choose([1, 2, 3], pr = 'equal')
# Rule K: If REVIEW == 1 THEN ASSIGN = choose([4, 5, 6], pr = 'equal')
# Rule L: If ESTAB_N > 1 THEN MU = True
# Rule M: If MU == True THEN DELETE

g1 = graph_from_literal(a--+c, a--+c, b--+a, b--+c, a--+d, c--+d, d--+e, f, g--+h, h--+i, i--+g, e--+j, e--+k, l--+m)
plot(g1, edge.arrow.size=.5, vertex.label.color='black')

# Node Classification
V(g1)$type = c("Intermediary", "Intermediary", "Progenitor", "Intermediary", "Intermediary", "Isolated", "Intermediary", "Intermediary", "Intermediary", "Terminal", "Terminal", "Progenitor", "Terminal")
pal <- brewer.pal(length(unique(V(g1)$type)), "Accent")
plot(g1, edge.arrow.size=.5, vertex.label.color='black', vertex.color=pal[as.numeric(as.factor(vertex_attr(g1, "type")))])

# Process Identification
ceb <- cluster_edge_betweenness(g1)
plot(ceb, g1, edge.arrow.size=.5, vertex.label.color='black')

# Node Importance
hs <- hub_score(g1, weights=NA)$vector
as <- authority_score(g1, weights=NA)$vector
# Degree Plot
plot(g1, edge.arrow.size=.5, vertex.label.color='black', vertex.color=pal[as.numeric(as.factor(vertex_attr(g1, "type")))], vertex.size = (degree(g1, mode = 'in') + 1)*7)

# Authority Score Plot
plot(g1, edge.arrow.size=.5, vertex.label.color='black', vertex.color=pal[as.numeric(as.factor(vertex_attr(g1, "type")))], vertex.size = as*10)

