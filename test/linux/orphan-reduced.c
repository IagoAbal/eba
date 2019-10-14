// SPDX-License-Identifier: GPL-2.0-only
/*
 * This file is part of UBIFS.
 *
 * Copyright (C) 2006-2008 Nokia Corporation.
 *
 * Author: Adrian Hunter
 */

#include "ubifs.h"

static void orphan_delete(struct ubifs_info *c, struct ubifs_orphan *orph)
{
	if (orph->del) {
		spin_unlock(&c->orphan_lock);
		return;
	}

	if (orph->cmt) {
		orph->del = 1;
		orph->dnext = c->orph_dnext;
		c->orph_dnext = orph;
		spin_unlock(&c->orphan_lock);
		return;
	}
}

void ubifs_delete_orphan(struct ubifs_info *c, ino_t inum)
{
	struct ubifs_orphan *orph, *child_orph, *tmp_o;

	spin_lock(&c->orphan_lock);

	orph = lookup_orphan(c, inum);
	// if (!orph) {
	// 	spin_unlock(&c->orphan_lock);
	// 	return;
	// }

	// list_for_each_entry_safe(child_orph, tmp_o, &orph->child_list, child_list) {
	// 	list_del(&child_orph->child_list);
	// 	orphan_delete(c, child_orph);
	// }
	
	orphan_delete(c, orph);
	spin_unlock(&c->orphan_lock);
}
