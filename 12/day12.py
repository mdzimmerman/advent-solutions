import json

f = open("input.txt", "r")
j = json.load(f)

def traverse(node):
    n = 0
    if (type(node) is list):
        for item in node:
            n += traverse(item)
    elif (type(node) is dict):
        hasRed = False
        for attr, value in node.iteritems():
            if value == "red":
                hasRed = True
        if hasRed:
            pass
        else:
            for attr, value in node.iteritems():
                n += traverse(value)
    else:
        if (type(node) is int):
            n += node
    return n
        #print node

print(traverse(j))
