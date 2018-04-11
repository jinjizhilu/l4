import os

exe_file = "..\\debug\\l4.exe"
test_files = os.listdir("test")

for name in test_files:
	print(name)
	os.system(exe_file + " test\\" + name)
	#os.system("pause")
os.system("pause")