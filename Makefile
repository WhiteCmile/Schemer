test:
	python3 ./build/build.py $(TASK_NAME)

clean:
	rm test.scm output/*

a8:
	$(MAKE) test TASK_NAME=a8