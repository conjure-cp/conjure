import sqlite3
from sqlite3 import Error
 

conn = sqlite3.connect("/home/tom/minion-private/build/gears/test.db")
cur = conn.cursor()
cur.execute("select nodeId, parentId from Node where parentId >= 0;")

rows = cur.fetchall()

d = {}
count = {}
count[0] = 0

for row in rows:
    d[int(row[0])] = int(row[1])
    count[int(row[0])] = 0

for nodeId in d.keys():

    while True:
        count[nodeId] = count[nodeId] + 1
        if not nodeId in d:
            break
        else:
            nodeId = d[nodeId]

    # print(nodeId)
 
print(count[0])
 