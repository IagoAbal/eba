

struct preempt_notifier;

struct preempt_ops {
  struct preempt_notifier *notifier;
};

struct preempt_notifier {
 struct preempt_ops *ops;
};

void preempt_notifier_register(struct preempt_notifier *notifier);
