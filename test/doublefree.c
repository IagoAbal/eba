
extern void kfree(void *);

void f(int *ip) {
	if (ip)
		(*ip)++;
}

int main() {
	void* ptr;
	kfree(ptr);
	kfree(ptr);
	return 0;
}
