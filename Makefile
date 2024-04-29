test:
	python3 ./build/build.py $(TASK_NAME)

clean:
	rm test.scm output/*

a2:
	$(MAKE) test TASK_NAME=a2