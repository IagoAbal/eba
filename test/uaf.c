
extern void kfree(void *);

void f(int *ip) {
	if (ip)
		(*ip)++;
}

int main() {
	void* ptr;
	kfree(ptr);
	f((int*)ptr);
	return 0;
}
