
import sys, json

edgeCovers = set()
clique3s = set()

with open(sys.argv[1], "r") as f:
    d = json.load(f)
    for i, iObj in d["covering"].items():
        for j, jObj in iObj.items():
            if "edgeCover" in jObj.keys():
                edgeCovers.add(tuple(sorted([int(i),int(j)])))
            if "clique3" in jObj.keys():
                k = jObj["clique3"]
                clique3s.add(tuple(sorted([int(i),int(j),int(k)])))

print("edges:", sorted(edgeCovers))
print("3 cliques:", sorted(clique3s))
