int lock1;
int lock2;

void extern _spin_unlock(void *);
void extern _spin_lock(void *);

void double_lock(int flag)
{
	if (flag) {
		_spin_lock(&lock1);
	}
}

int main()
{
	_spin_lock(&lock2);
	_spin_lock(&lock1);
	double_lock(1);
	_spin_unlock(&lock2);
}