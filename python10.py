# JP Onnela
# April 20, 2021

# Edited May 12, 2021 by Jonathan Larson

import networkx as nx
import random
import scipy.stats as ss
import time

def generate_DMC(q_mod, q_con, n):
    """Generate DMC model realization given parameters."""
    G = nx.Graph()
    G.add_edge(0,1)
    new_nodes = list(range(2,n))
    anchor_nodes = []
    for v in new_nodes:
        u = random.choice(list(G.nodes()))
        anchor_nodes.append(u)
        G.add_node(v)
        
        # duplication
        G.add_edges_from([(v,w) for w in G.neighbors(u)])
        
        # mutation
        for w in list(G.neighbors(u)):
            if ss.bernoulli.rvs(q_mod):
                edge = random.choice([(v,w), (u,w)])
                G.remove_edge(*edge)
        
        # complementation
        if ss.bernoulli.rvs(q_con):
            G.add_edge(u,v)
    return (G, new_nodes, anchor_nodes)


def deconstruct_DMC(G, alpha, beta):
    """Deconstruct a DMC graph over a single step."""
    # reverse complementation
    if G.has_edge(alpha, beta):
        G.remove_edge(alpha, beta)
        w = 1
    else:
        w = 0

    # reverse mutation
    alpha_neighbors = set(G.neighbors(alpha))
    beta_neighbors = set(G.neighbors(beta))
    x = len(alpha_neighbors & beta_neighbors)
    y = len(alpha_neighbors | beta_neighbors)
    for neighbor in alpha_neighbors:
        G.add_edge(beta, neighbor)

    # reverse duplication
    G.remove_node(alpha)
    return (w, x, y)


def find_min_uni_pair(G):
    """Find pair of nodes that have minimal cardinality of the union of their neighbors."""
    alpha = None
    beta = None
    union_size = G.number_of_nodes()
    nodes = list(G.nodes())
    random.shuffle(nodes)
    for u in nodes:
        for v in nodes:
            if u > v:
                u_neighbors = set(G.neighbors(u))
                v_neighbors = set(G.neighbors(v))
                y = len(u_neighbors | v_neighbors)
                if G.has_edge(u,v):
                    y = y - 2
                if y < union_size:
                    union_size = y
                    alpha = u
                    beta = v
    return (alpha, beta, union_size)


def deconstruct(G):
    """Deconstruct the graph until."""
    alphas = []
    betas = []
    W = 0
    X = 0
    Y = 0
    (alpha, beta, union_size) = find_min_uni_pair(G)
    while (not alpha is None and not beta is None):
        print("Number of nodes remaining:", G.number_of_nodes())
        alphas.append(alpha)
        betas.append(beta)
        (w, x, y) = deconstruct_DMC(G, alpha, beta)
        W += w
        X += x
        Y += y
        (alpha, beta, union_size) = find_min_uni_pair(G)
    return (alphas, betas, W, X, Y)


def estimate_parms(W, X, Y, n):
    """Compute estimates of q_mod and q_con parameters."""
    q_mod_hat = 1 - X / Y
    q_con_hat = W / (n - 1)
    return (q_mod_hat, q_con_hat)


def read_edgelist(input_file):
    """Read edgelist from input file"""
    G = nx.Graph()
    counter = 0
    for line in open(input_file):
        counter += 1
        line = line.rstrip().split("\t")
        node_i = line[0]
        node_j = line[1]
        G.add_edge(node_i, node_j)
    return (G, counter)


def print_stats(G, new_nodes, anchor_nodes):
    """Print out some statistics."""
    print("Nodes:", G.nodes())
    print("Edges:", G.edges())
    print("New nodes (alpha):", new_nodes)
    print("Anchor nodes (beta):", anchor_nodes)

def save_results(output_file):
	F = open(output_file, "w")
	# alphas
	for alpha in alphas:
		F.write(str(alpha) + " ")
	F.write("\n")	
	# betas
	for beta in betas:
		F.write(str(beta) + " ")
	F.write("\n")	
	# others
	F.write(str(W) + " " + str(X) + " " + str(Y) + " " + str(q_mod_hat) + " " + str(q_con_hat))
	F.close()


# ----------------------------------------------------------------


# input and output files
input_file = "HuRI.tsv"

# read data
(G, counter) = read_edgelist(input_file)

# sample nodes
sampled_nodes = random.sample(list(G.nodes()),round(0.10 * G.number_of_nodes()))
G.remove_nodes_from([n for n in G if n not in set(sampled_nodes)])
G.remove_edges_from(nx.selfloop_edges(G))
print(G.number_of_edges())

# degenerate graph
n = G.number_of_nodes()
start = time.time()
(alphas, betas, W, X, Y) = deconstruct(G)
end = time.time()
print("Time elapsed:", end - start)
(q_mod_hat, q_con_hat) = estimate_parms(W, X, Y, n)
print("Parameter estimates:", q_mod_hat, q_con_hat)




