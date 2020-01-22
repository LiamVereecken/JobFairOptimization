import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Read in companies
companies = pd.read_csv("companies.csv",sep=';')
companies.columns=["company","sector","exclusive"]
companies["company"]= companies["exclusive"] == "ja"
print(companies)

print(companies["sector"].value_counts())

# Create lots
width = 11
height = 8
#print(np.array(range(width)))
#print([ele for ele in range(width) for ele in range(height)])
#print(np.repeat(np.array(range(height)),width))
lots = pd.DataFrame({"n":range(width*height),
                    "x":[ele for ele in range(height) for ele in range(width)],
                    "y":np.repeat(np.array(range(height)),width)},
                    columns=["x","y"])

plt.scatter(lots["x"],lots["y"])
plt.show()

print(lots)

def canSee(x1,y1,x2,y2):
    if ()
    return True
