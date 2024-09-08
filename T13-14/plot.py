import pandas as pd
import matplotlib.pyplot as plt

data = pd.read_csv('RK.csv', header=None, names=['Time', 'Position', 'Speed'])

plt.figure(figsize=(12, 6))

plt.subplot(2, 1, 1)
plt.plot(data['Time'], data['Position'], label='Position', color='b')
plt.xlabel('Time')
plt.ylabel('Position')
plt.title('Position vs. Time')
plt.legend()
plt.grid(True)

plt.subplot(2, 1, 2)
plt.plot(data['Time'], data['Speed'], label='Speed', color='r')
plt.xlabel('Time')
plt.ylabel('Speed')
plt.title('Speed vs. Time')
plt.legend()
plt.grid(True)

plt.tight_layout()
plt.show()
