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
