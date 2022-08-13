---
title: "Updating a Many to Many relationship"
slug: "updating-a-many-to-many-relationship"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

How to update a Many to Many relationship in EF Core:



## MVC POST Edit example
Say we have a Product class with Multiple Colors which can be on many Products.


    public class Product
    {       
        public int ProductId { get; set; }
        public ICollection<ColorProduct> ColorProducts { get; set; }
    }

    public class ColorProduct
    {
        public int ProductId { get; set; }
        public int ColorId { get; set; }

        public virtual Color Color { get; set; }
        public virtual Product Product { get; set; }
    }

    public class Color
    {      
        public int ColorId { get; set; }
        public ICollection<ColorProduct> ColorProducts { get; set; }
    }

Using this extension to make it easier:

    public static class Extensions
    {
        public static void TryUpdateManyToMany<T, TKey>(this DbContext db, IEnumerable<T> currentItems, IEnumerable<T> newItems, Func<T, TKey> getKey) where T : class
        {
            db.Set<T>().RemoveRange(currentItems.Except(newItems, getKey));
            db.Set<T>().AddRange(newItems.Except(currentItems, getKey));
        }

        public static IEnumerable<T> Except<T, TKey>(this IEnumerable<T> items, IEnumerable<T> other, Func<T, TKey> getKeyFunc)
        {
            return items
                .GroupJoin(other, getKeyFunc, getKeyFunc, (item, tempItems) => new { item, tempItems })
                .SelectMany(t => t.tempItems.DefaultIfEmpty(), (t, temp) => new { t, temp })
                .Where(t => ReferenceEquals(null, t.temp) || t.temp.Equals(default(T)))
                .Select(t => t.t.item);
        }
    }

Updating a product's colors would look like this (a MVC Edit POST Method)

    [HttpPost]
    public IActionResult Edit(ProductVm vm)
    {
    if (ModelState.IsValid)
        {
            var model = db.Products
                .Include(x => x.ColorProducts)                    
                .FirstOrDefault(x => x.ProductId == vm.Product.ProductId);
            
            db.TryUpdateManyToMany(model.ColorProducts, vm.ColorsSelected
                .Select(x => new ColorProduct
                {
                    ColorId = x,
                    ProductId = vm.Product.ProductId
                }), x => x.ColorId);

            db.SaveChanges();

            return RedirectToAction("Index");
        }   
       return View(vm);
    }


    public class ProductVm
    {          
        public Product Product { get; set; }
        
        public IEnumerable<int> ColorsSelected { get; set; }      
    }


Code has been simplified as much as i can, no extra properties on any classes. 

