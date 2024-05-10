test:
	python3 ./build/build.py $(TASK_NAME)

a1:
	$(MAKE) test TASK_NAME=a1

clean:
	rm test.scm output/*

<<<<<<< HEAD
a2:
	$(MAKE) test TASK_NAME=a2
a3:
	$(MAKE) test TASK_NAME=a3
a4:
	$(MAKE) test TASK_NAME=a4
=======
a5:
	$(MAKE) test TASK_NAME=a5
>>>>>>> origin/a5
