struct spinlock { };

extern void _raw_spin_lock(struct spinlock *);
extern void _raw_spin_unlock(struct spinlock *);

extern void spin_lock(struct spinlock *lock) { _raw_spin_lock(lock); }
extern void spin_unlock(struct spinlock *lock) { _raw_spin_unlock(lock); }

__attribute__ ((noinline)) int nondet() { return 42; }

struct ocrdma_cq {
	struct spinlock cq_lock;
};

struct ocrdma_qp {
	struct ocrdma_cq *sq_cq;
	struct ocrdma_cq *rq_cq;
};

int ocrdma_destroy_qp()
{
	struct ocrdma_qp *qp;

	if (qp->rq_cq && (qp->rq_cq != qp->sq_cq))
		spin_lock(&qp->rq_cq->cq_lock);

	if (qp->rq_cq && (qp->rq_cq != qp->sq_cq))
		spin_unlock(&qp->rq_cq->cq_lock);

	spin_lock(&qp->rq_cq->cq_lock);

	return 0;
}
