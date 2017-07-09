#include "monterko.h"

static int deviceids[MAX_DEVICES];
struct monter_device_data data[MAX_DEVICES];

int
monter_assign_id(void)
{
	int i = 0;
	for (; i < MAX_DEVICES; ++i)
		if (deviceids[i] == 0)
			break;
	
	deviceids[i] = 1;
	return i;
}

int
monter_ptr_to_devid(struct monter_device_data *ptr)
{
	return (ptr - data);
}

struct monter_device_data *
monter_find_devt(dev_t dev)
{
	int i = 0;
	for (; i < MAX_DEVICES; ++i)
		if (data[i].devt == dev)
			break;
	
	return data + i;
}

void
monter_free_id(int id)
{
	deviceids[id] = 0;
}

int
monter_probe(struct pci_dev *dev, const struct pci_device_id *id)
{
	int devid;
	
	mutex_lock(&globallock);
	
	devid = monter_assign_id();
	
	if (devid == MAX_DEVICES)
		goto probe_no_id;
	
	pci_set_drvdata(dev, (void *) (data + devid));
	
	if (alloc_chrdev_region(&(data[devid].devt), 0, 1, DEVICE_NAME) < 0)
		goto probe_free_id;
	
	cdev_init(&(data[devid].cdev), &monter_operations);
	
	if (cdev_add(&(data[devid].cdev), data[devid].devt, 1) < 0)
		goto probe_free_region;
	
	data[devid].device = device_create(monter_class, &(dev->dev),
		data[devid].devt, NULL, DEVICE_NAME "%d", devid);
	
	if (IS_ERR(data[devid].device))
		goto probe_cdev_del;
	
	if (pci_enable_device(dev) < 0)
		goto probe_device_destroy;
	
	if (pci_request_regions(dev, DEVICE_NAME))
		goto probe_disable_device;
	
	data[devid].bar = pci_iomap(dev, 0, MONTER_FIFO_STATE);
	if (data[devid].bar == NULL)
		goto probe_release_regions;
	
	pci_set_master(dev);
	
	if (pci_set_dma_mask(dev, DMA_BIT_MASK(32)))
		goto probe_clear_master;
	if (pci_set_consistent_dma_mask(dev, DMA_BIT_MASK(32)))
		goto probe_clear_master;
	
	// Pamięć na kolejkę poleceń
	data[devid].instructions = dma_zalloc_coherent(&dev->dev,
		(INSTRUCTION_LIMIT + 32) * 4, &(data[devid].inst_handle), 0);
	
	if (data[devid].instructions == NULL)
		goto probe_clear_master;
	
	data[devid].user = NULL;
	data[devid].active = 0;
	
	iowrite32(0, data[devid].bar + MONTER_ENABLE);
	iowrite32(MONTER_RESET_CALC | MONTER_RESET_FIFO, data[devid].bar + MONTER_RESET);
	iowrite32(0, data[devid].bar + MONTER_CMD_READ_PTR);
	iowrite32(0, data[devid].bar + MONTER_CMD_WRITE_PTR);
	iowrite32(MONTER_INTR_INVALID_CMD | MONTER_INTR_FIFO_OVERFLOW | MONTER_INTR_NOTIFY, data[devid].bar + MONTER_INTR);
	
	if (request_irq(dev->irq, monter_irq, IRQF_SHARED, DEVICE_NAME, (void *) (data + devid)))
		goto probe_irq_error;
	
	mutex_unlock(&globallock);
	printk(KERN_WARNING "monter: device detected\n");
	return 0;
	
	free_irq(dev->irq, (void *) (data + devid));
	
	probe_irq_error:
	iowrite32(0, data[devid].bar + MONTER_ENABLE);
	iowrite32(MONTER_RESET_CALC | MONTER_RESET_FIFO, data[devid].bar + MONTER_RESET);
	iowrite32(0, data[devid].bar + MONTER_CMD_READ_PTR);
	iowrite32(0, data[devid].bar + MONTER_CMD_WRITE_PTR);
	iowrite32(0, data[devid].bar + MONTER_INTR);
	
	dma_free_coherent(&dev->dev, (INSTRUCTION_LIMIT + 32) * 4,
		data[devid].instructions, data[devid].inst_handle);
	
	probe_clear_master:
	pci_clear_master(dev);
	
	//probe_pci_iounmap:
	pci_iounmap(dev, data[devid].bar);
	
	probe_release_regions:
	pci_release_regions(dev);
	
	probe_disable_device:
	pci_disable_device(dev);
	
	probe_device_destroy:
	device_destroy(monter_class, data[devid].devt);
	
	probe_cdev_del:
	cdev_del(&(data[devid].cdev));
	
	probe_free_region:
	unregister_chrdev_region(data[devid].devt, 1);
	
	probe_free_id:
	monter_free_id(devid);
	
	probe_no_id:
	mutex_unlock(&globallock);
	printk(KERN_WARNING "monter: error on device probe\n");
	
	return -1;
}

void 
monter_remove(struct pci_dev *dev)
{
	struct monter_device_data *devdata;
	int devid;
	
	mutex_lock(&globallock);
	
	devdata = (struct monter_device_data *) pci_get_drvdata(dev);
	devid = monter_ptr_to_devid(devdata);
	
	iowrite32(0, data[devid].bar + MONTER_ENABLE);
	iowrite32(MONTER_RESET_CALC | MONTER_RESET_FIFO, data[devid].bar + MONTER_RESET);
	iowrite32(0, data[devid].bar + MONTER_CMD_READ_PTR);
	iowrite32(0, data[devid].bar + MONTER_CMD_WRITE_PTR);
	iowrite32(0, data[devid].bar + MONTER_INTR);
	
	free_irq(dev->irq, (void *) devdata);
	dma_free_coherent(&dev->dev, (INSTRUCTION_LIMIT + 32) * 4,
		devdata->instructions, devdata->inst_handle);
	pci_clear_master(dev);
	pci_iounmap(dev, data[devid].bar);
	pci_release_regions(dev);
	pci_disable_device(dev);
	device_destroy(monter_class, data[devid].devt);
	cdev_del(&(data[devid].cdev));
	unregister_chrdev_region(data[devid].devt, 1);
	monter_free_id(devid);
	
	mutex_unlock(&globallock);
	printk(KERN_WARNING "monter: unloading device\n");
}

irqreturn_t
monter_irq(int irq, void *dev)
{
	struct monter_device_data *data;
	unsigned int flags;

	data = (struct monter_device_data *) dev;
	flags = ioread32(data->bar + MONTER_INTR);
	
	if (flags & MONTER_INTR_INVALID_CMD) {
		iowrite32(MONTER_INTR_INVALID_CMD, data->bar + MONTER_INTR);
		return IRQ_HANDLED;
	}
	
	if (flags & MONTER_INTR_FIFO_OVERFLOW) {
		iowrite32(MONTER_INTR_FIFO_OVERFLOW, data->bar + MONTER_INTR);
		return IRQ_HANDLED;
	}
	
	if (flags & MONTER_INTR_NOTIFY) {
		spin_lock_irqsave(&spinlock, spinflags);
		iowrite32(MONTER_INTR_NOTIFY, data->bar + MONTER_INTR);
		
		if (data->user == NULL) {
			spin_unlock_irqrestore(&spinlock, spinflags);
			return IRQ_HANDLED;
		}
		
		iowrite32(0, data->bar + MONTER_ENABLE);
		iowrite32(MONTER_RESET_CALC | MONTER_RESET_FIFO, data->bar + MONTER_RESET);
		
		// jeśli wykonano wszystkie operacje i kolejka do zapisu jest pusta
		if (data->user->write_wait == 0 && data->user->used == 0) {
			complete(&data->user->fsync);
			data->user->fsync_counter = 0;
		}
		
		complete(&data->user->buffer_wait);
		//data->user = list_entry(data->user->users.next, struct monter_context, users);
		data->active = 0;
		
		// przejście po wszystkich kontekstach od następnego i wybranie tego z niepustymi
		// instrukcjami.
		if (monter_select_context(data->user)) {
			monter_run_context(data->user);
		}
		
		spin_unlock_irqrestore(&spinlock, spinflags);
		
		return IRQ_HANDLED;
	}
	
	return IRQ_NONE;
}
