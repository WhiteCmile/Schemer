test:
	python3 ./build/build.py $(TASK_NAME)

a1:
	$(MAKE) test TASK_NAME=a1

clean:
	rm test.scm output/*

a2:
	$(MAKE) test TASK_NAME=a2
a3:
	$(MAKE) test TASK_NAME=a3
a4:
	$(MAKE) test TASK_NAME=a4
a5:
	$(MAKE) test TASK_NAME=a5
a6:
	$(MAKE) test TASK_NAME=a6
a7:
	$(MAKE) test TASK_NAME=a7
a8:
	$(MAKE) test TASK_NAME=a8
a9:
	$(MAKE) test TASK_NAME=a9
a10:
	$(MAKE) test TASK_NAME=a10
a11:
	$(MAKE) test TASK_NAME=a11