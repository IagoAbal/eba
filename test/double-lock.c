int lock1;
int lock2;
int lock3;

void extern _spin_unlock(void *);
void extern _spin_lock(void *);

void double_lock(int flag)
{
	if (flag) {
		_spin_lock(&lock1);
	}
	else {
		_spin_lock(&lock2);
	}
}

int main()
{
	_spin_lock(&lock1);
	_spin_lock(&lock2);
	_spin_lock(&lock3);
	double_lock(1);
	_spin_unlock(&lock3);
}