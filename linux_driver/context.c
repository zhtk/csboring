#include "monterko.h"

int
monter_open(struct inode *i, struct file *fp)
{
	struct monter_device_data *dev;
	struct monter_context *ct;
	
	mutex_lock(&globallock);
	
	dev = monter_find_devt(i->i_rdev);
	ct = kmem_cache_alloc(context_alloc, 0);
	fp->private_data = ct;
	
	if (ct == NULL) {
		mutex_unlock(&globallock);
		return -ENOMEM;
	}
	
	ct->device = dev;
	INIT_LIST_HEAD(&ct->users);
	ct->used = 0;
	ct->work = NULL;
	init_completion(&ct->fsync);
	complete(&ct->fsync);
	init_completion(&ct->buffer_wait);
	complete(&ct->buffer_wait);
	mutex_init(&ct->write_mutex);
	ct->fsync_counter = 0;
	ct->write_wait = 0;
	ct->addr_set = 0;
	ct->reg_a = 0;
	ct->reg_b = 0;
	ct->new_reg_a = 0;
	ct->new_reg_b = 0;
	
	spin_lock_irqsave(&spinlock, spinflags);
	
	if (dev->user == NULL)
		dev->user = ct;
	else
		list_add_tail(&(ct->users), &(dev->user->users));
	
	spin_unlock_irqrestore(&spinlock, spinflags);
	
	mutex_unlock(&globallock);
	return 0;
}

int
monter_release(struct inode *inode, struct file *fp)
{
	struct monter_context *ct;
	ct = (struct monter_context *) fp->private_data;
	
	mutex_lock(&globallock);
	spin_lock_irqsave(&spinlock, spinflags);
	
	if (list_empty(&ct->users)) {
		iowrite32(0, ct->device->bar + MONTER_ENABLE);
		iowrite32(MONTER_RESET_CALC | MONTER_RESET_FIFO, ct->device->bar + MONTER_RESET);
		iowrite32(0, ct->device->bar + MONTER_CMD_READ_PTR);
		iowrite32(0, ct->device->bar + MONTER_CMD_WRITE_PTR);
		
		ct->device->user = NULL;
		ct->device->active = 0;
	} else {
		// jeśli obecnie prowadzona jest operacja z tego ct to zresetuj urządzenie
		// i zmień kontekst
		if (ct->device->user == ct) {
			iowrite32(0, ct->device->bar + MONTER_ENABLE);
			iowrite32(MONTER_RESET_CALC | MONTER_RESET_FIFO, ct->device->bar + MONTER_RESET);
			iowrite32(0, ct->device->bar + MONTER_CMD_READ_PTR);
			iowrite32(0, ct->device->bar + MONTER_CMD_WRITE_PTR);
			ct->device->active = 0;
			ct->device->user = list_entry(ct->users.next, struct monter_context, users);
		}
		
		// usunięcie kontekstu z listy
		list_del(&ct->users);
		
		// uruchom urządzenie na nowo (jeśli warto)
		if (ct->device->active == 0 && monter_select_context(ct->device->user))
			monter_run_context(ct->device->user);
	}
	
	spin_unlock_irqrestore(&spinlock, spinflags);
	
	complete(&ct->fsync);
	if (ct->work != NULL)
		dma_free_coherent(ct->device->device->parent, 64*1024,
			ct->work, ct->work_handle);
	
	kmem_cache_free(context_alloc, fp->private_data);
	
	mutex_unlock(&globallock);
	return 0;
}

long
monter_ioctl(struct file *fp, unsigned int cmd, unsigned long arg)
{
	struct monter_context *ct;
	ct = (struct monter_context *) fp->private_data;
	
	if (cmd != MONTER_IOCTL_SET_SIZE)
		return -ENOTTY;
	
	if (arg == 0 || arg > 64*1024 || arg % 4096 != 0)
		return -EINVAL;
	
	mutex_lock(&globallock);
	
	if (ct->work != NULL) {
		mutex_unlock(&globallock);
		return -EINVAL;
	}
	
	// Trudny wybór między pamięcią a bezpieczeństwem
	//ct->work = dma_zalloc_coherent(ct->device->device->parent, arg, &(ct->work_handle), 0);
	ct->work = dma_zalloc_coherent(ct->device->device->parent, 64*1024, &(ct->work_handle), 0);
	ct->work_size = arg;
	
	if (ct->work == NULL) {
		mutex_unlock(&globallock);
		return -ENOMEM;
	}
	
	mutex_unlock(&globallock);
	return 0;
}

int
monter_mmap(struct file *fp, struct vm_area_struct *vma)
{
	unsigned long off;
	unsigned long vsize;
	unsigned long psize;
	struct monter_context *ct;
	int res;
	
	ct = (struct monter_context *) fp->private_data;

	if (!(vma->vm_flags & VM_SHARED))
		return -EINVAL;

	mutex_lock(&globallock);
	
	if (ct->work == NULL) {
		mutex_unlock(&globallock);
		return -EINVAL;
	}

	off = vma->vm_pgoff << PAGE_SHIFT;
	vsize = vma->vm_end - vma->vm_start;
	psize = ct->work_size - off;
	
	if (vsize > psize) {
		mutex_unlock(&globallock);
		return -EINVAL;
	}

	res = dma_common_mmap(ct->device->device->parent, vma, ct->work,
		ct->work_handle, vma->vm_end - vma->vm_start);
	
	mutex_unlock(&globallock);
	return res;
}

static int
check_operation(int op)
{
	switch (MONTER_SWCMD_TYPE(op)) {
		case MONTER_SWCMD_TYPE_ADDR_AB:
			return 0;
		case MONTER_SWCMD_TYPE_RUN_MULT:
			return op & (1 << 17);
		case MONTER_SWCMD_TYPE_RUN_REDC:
			return op & (1 << 17);
		default:
			return 1;
	}
}

static int
convert_operation(int op)
{
	int a, b, d, m;
	
	switch (MONTER_SWCMD_TYPE(op)) {
		case MONTER_SWCMD_TYPE_ADDR_AB:
			a = MONTER_SWCMD_ADDR_A(op);
			b = MONTER_SWCMD_ADDR_B(op);
			return MONTER_CMD_ADDR_AB(a, b, 0);
		case MONTER_SWCMD_TYPE_RUN_MULT:
			m = MONTER_SWCMD_RUN_SIZE(op);
			d = MONTER_SWCMD_ADDR_D(op);
			return MONTER_CMD_RUN_MULT(m, d, 0);
		case MONTER_SWCMD_TYPE_RUN_REDC:
			m = MONTER_SWCMD_RUN_SIZE(op);
			d = MONTER_SWCMD_ADDR_D(op);
			return MONTER_CMD_RUN_REDC(m, d, 0);
		default:
			return 0;
	}
}

static int
check_monter_command(int cmd, int *addr_set, int work_size, int *reg_a, int *reg_b)
{
	if (cmd == 0)
		return 1;

	if (MONTER_CMD_KIND(cmd) == MONTER_CMD_KIND_ADDR_AB)
		*addr_set = 1;
	
	if (*addr_set == 0 && MONTER_CMD_KIND(cmd) == MONTER_CMD_KIND_RUN)
		return 1;
			
	// Sprawdzenie adresów - TODO
	if (MONTER_CMD_KIND(cmd) == MONTER_CMD_KIND_ADDR_AB) {
		if (MONTER_CMD_ADDR_A(cmd) > work_size)
			return 1;

		if (MONTER_CMD_ADDR_B(cmd) > work_size)
			return 1;
				
		*reg_a = MONTER_CMD_ADDR_A(cmd);
		*reg_b = MONTER_CMD_ADDR_B(cmd);
	} else if (MONTER_CMD_KIND(cmd) == MONTER_CMD_KIND_RUN) {
		int reg_d, run_size;
		reg_d = MONTER_CMD_ADDR_D(cmd);
		run_size = MONTER_CMD_RUN_SIZE(cmd);
				
		if (reg_d > work_size)
			return 1;
		
		//run_size += 1;
		run_size *= 4;
		
		if (*reg_b + run_size > work_size)
			return 1;
					
		if (reg_d + run_size * 2 > work_size)
			return 1;
		
		if (MONTER_CMD_SUBTYPE(cmd) == MONTER_CMD_SUBTYPE_RUN_MULT) {
			if (*reg_a + run_size > work_size)
				return 1;
		} else if (MONTER_CMD_SUBTYPE(cmd) == MONTER_CMD_SUBTYPE_RUN_REDC) {
			if (*reg_a + 4 > work_size)
				return 1;
		} else {
			return 1;
		}
	} else {
		return 1;
	}
	
	return 0;
}

int
monter_select_context(struct monter_context *ct)
{
	struct monter_context *tmp;
	struct list_head *i;
	
	// list_for_each_entry(tmp, &ct->users, users)
	list_for_each(i, &ct->users) {
		tmp = list_entry(i, struct monter_context, users);
		
		if (tmp->used) {
			tmp->device->user = tmp;
			return 1;
		}
	}
	
	if (ct->used) {
		ct->device->user = ct;
		return 1;
	}
	
	return 0;
}

void
monter_run_context(struct monter_context *ct)
{
	int *queue;
	int i;
	
	queue = (int *) ct->device->instructions;
	
	iowrite32(0, ct->device->bar + MONTER_ENABLE);
	iowrite32(MONTER_RESET_CALC | MONTER_RESET_FIFO, ct->device->bar + MONTER_RESET);
	
	for (i = 0; i < 16; ++i)
		queue[i] = MONTER_CMD_PAGE(i, MONTER_CMD_PAGE_ADDR(ct->work_handle + 4096*i), 0);
	
	queue[16] = MONTER_CMD_ADDR_AB(ct->reg_a, ct->reg_b, 0);
	
	for (i = 0; i < ct->used; ++i) {
		// Aktualizacja adresów
		if (MONTER_CMD_KIND(ct->inst[i]) == MONTER_CMD_KIND_ADDR_AB) {
			ct->reg_a = MONTER_CMD_ADDR_A(ct->inst[i]);
			ct->reg_b = MONTER_CMD_ADDR_B(ct->inst[i]);
		}
		
		// Wpisanie do kolejki
		queue[17 + i] = ct->inst[i];
	}
	
	queue[17 + i] = MONTER_CMD_COUNTER(0, 1);
	
	iowrite32(ct->device->inst_handle, ct->device->bar + MONTER_CMD_READ_PTR);
	iowrite32(ct->device->inst_handle + 4*(18 + i), ct->device->bar + MONTER_CMD_WRITE_PTR);
	iowrite32(MONTER_ENABLE_CALC | MONTER_ENABLE_FETCH_CMD, ct->device->bar + MONTER_ENABLE);
	iowrite32(MONTER_INTR_NOTIFY, ct->device->bar + MONTER_INTR_ENABLE);
	
	ct->device->active = 1;
	ct->device->user = ct;
	ct->used = 0;
	complete(&ct->buffer_wait);
}

ssize_t
monter_write(struct file *fp, const char __user *buff, size_t size, loff_t *off)
{
	struct monter_context *ct;
	int instbuff[INSTRUCTION_LIMIT];
	size_t i, j;
	int addr_set, work_size, reg_a, reg_b;
	
	ct = (struct monter_context *) fp->private_data;
	
	if (size % 4)
		return -EINVAL;
	
	mutex_lock(&globallock);
	
	if (ct->work == NULL) {
		mutex_unlock(&globallock);
		return -EINVAL;
	}
	
	work_size = ct->work_size;
	
	mutex_unlock(&globallock);
	mutex_lock(&ct->write_mutex);
	
	spin_lock_irqsave(&spinlock, spinflags);
	ct->write_wait++;
	spin_unlock_irqrestore(&spinlock, spinflags);
	addr_set = ct->addr_set;
	reg_a = ct->new_reg_a;
	reg_b = ct->new_reg_b;
	
	// Sprawdzenie poprawności poleceń
	i = 0;
	while (i < size) {
		size_t cp;
		cp = (i + INSTRUCTION_LIMIT * 4 <= size) ? (INSTRUCTION_LIMIT * 4) : (size - i);

		if (copy_from_user((void *) instbuff, buff + i, cp)) {
			spin_lock_irqsave(&spinlock, spinflags);
			ct->write_wait--;
			spin_unlock_irqrestore(&spinlock, spinflags);
			mutex_unlock(&ct->write_mutex);
			return -EINVAL;
		}
		
		for (j = 0; j < cp / 4; ++j) {
			if (check_operation(instbuff[j])) {
				spin_lock_irqsave(&spinlock, spinflags);
				ct->write_wait--;
				spin_unlock_irqrestore(&spinlock, spinflags);
				mutex_unlock(&ct->write_mutex);
				return -EINVAL;
			}
			
			instbuff[j] = convert_operation(instbuff[j]);
			
			if (check_monter_command(instbuff[j], &addr_set, work_size, &reg_a, &reg_b)) {
				spin_lock_irqsave(&spinlock, spinflags);
				ct->write_wait--;
				spin_unlock_irqrestore(&spinlock, spinflags);
				mutex_unlock(&ct->write_mutex);
				return -EINVAL;
			}
		}
		
		i += cp;
	}
	
	// Przepisanie poleceń do bufora
	
	ct->addr_set = addr_set;
	ct->new_reg_a = reg_a;
	ct->new_reg_b = reg_b;
	
	i = 0;
	while (i < size) {
		size_t cp;
		
		spin_lock_irqsave(&spinlock, spinflags);
		
		if (i + (INSTRUCTION_LIMIT - ct->used) * 4 <= size) {
			cp = (INSTRUCTION_LIMIT - ct->used) * 4;
		} else {
			cp = size - i;
		}
		
		spin_unlock_irqrestore(&spinlock, spinflags);
		
		if (copy_from_user((void *) instbuff, buff + i, cp))
			printk(KERN_WARNING "monter: strange thing during write\n");
		
		spin_lock_irqsave(&spinlock, spinflags);
		for (j = 0; j < cp / 4; ++j)
			ct->inst[ct->used + j] = convert_operation(instbuff[j]);
		
		ct->used += cp / 4;
		
		i += cp;
		if (i < size)
			init_completion(&ct->buffer_wait);
		
		// włączenie urządzenia jeśli było wyłączone i jest taka potrzeba
		if (ct->device->active == 0) {
			monter_run_context(ct);
		}
		
		spin_unlock_irqrestore(&spinlock, spinflags);
		
		if (i < size)
			wait_for_completion(&ct->buffer_wait);
	}
	
	mutex_unlock(&ct->write_mutex);
	spin_lock_irqsave(&spinlock, spinflags);
	ct->write_wait--;
	spin_unlock_irqrestore(&spinlock, spinflags);
	
	return size;
}

int
monter_fsync(struct file *fp, loff_t l1, loff_t l2, int datasync)
{
	struct monter_context *ct;
	ct = (struct monter_context *) fp->private_data;
	
	spin_lock_irqsave(&spinlock, spinflags);
	if (ct->write_wait == 0 && ct->used == 0 && (!ct->device->active || ct->device->user != ct)) {
		spin_unlock_irqrestore(&spinlock, spinflags);
		return 0;
	}
	
	if (ct->fsync_counter == 0)
		init_completion(&ct->fsync);
	ct->fsync_counter++;
	spin_unlock_irqrestore(&spinlock, spinflags);
	
	wait_for_completion(&ct->fsync);
	
	return 0;
}
