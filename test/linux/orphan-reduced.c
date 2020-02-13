static void orphan_delete(struct ubifs_info *c, struct ubifs_orphan *orph)
{
	if (orph->del) {
		spin_unlock(&c->orphan_lock);
		return;
	}
	if (orph->cmt) {
		spin_unlock(&c->orphan_lock);
		return;
	}
}

void ubifs_delete_orphan(struct ubifs_info *c, ino_t inum)
{
	orphan_delete(c, orph);
	spin_unlock(&c->orphan_lock);
}
