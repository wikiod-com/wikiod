---
title: "Repository Pattern"
slug: "repository-pattern"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

About the implementation of **`IEnumerable<TEntity> Get(Expression<Func<TEntity, bool>> filter)`**: The idea of this is to use Expressions like `i => x.id == 17` to write generic requests. It is a way to query data without using the specific query language of your technology. The implementation is rather extensive, therefore you might want to consider other alternatives, like specific methods on your implemented repositories: An imaginary `CompanyRepository` could provide the method `GetByName(string name)`.

## Read-only repositories (C#)
A repository pattern can be used to encapsulate data storage specific code in designated components. The part of your application, that needs the data will only work with the repositories. You will want to create a repository for each combination of item you store and your database technology.

Read only repositories can be used to create repositories that are not allowed to manipulate data.

# The interfaces

    public interface IReadOnlyRepository<TEntity, TKey> : IRepository
    {
        IEnumerable<TEntity> Get(Expression<Func<TEntity, bool>> filter);

        TEntity Get(TKey id);
    }

    public interface IRepository<TEntity, TKey> : IReadOnlyRepository<TEntity, TKey>
    {
        TKey Add(TEntity entity);

        bool Delete(TKey id);

        TEntity Update(TKey id, TEntity entity);
    }

# An example implementation using ElasticSearch as technology (with NEST)

    public abstract class ElasticReadRepository<TModel> : IReadOnlyRepository<TModel, string>
        where TModel : class
    {

        protected ElasticClient Client;

        public ElasticReadRepository()
        {
            Client = Connect();
        }

        protected abstract ElasticClient Connect();

        public TModel Get(string id)
        {
            return Client.Get<TModel>(id).Source;
        }

        public IEnumerable<TModel> Get(Expression<Func<TModel, bool>> filter)
        {
            /* To much code for this example */
            throw new NotImplementedException();
        }
    }

    public abstract class ElasticRepository<TModel>
        : ElasticReadRepository<TModel>, IRepository<TModel, string>
        where TModel : class
    {
        public string Add(TModel entity)
        {
            return Client.Index(entity).Id;
        }

        public bool Delete(string id)
        {
            return Client.Delete<TModel>(id).Found;
        }

        public TModel Update(string id, TModel entity)
        {
            return Connector.Client.Update<TModel>(
                id,
                update => update.Doc(entity)
            ).Get.Source;
        }
    }

Using this implementation, you can now create specific Repositories for the items you want to store or access. When using elastic search, it is common, that some components should only read the data, thus read-only repositories should be used.

## Repository Pattern using Entity Framework (C#)
Repository interface;

    public interface IRepository<T>
    {
        void Insert(T entity);
        void Insert(ICollection<T> entities);
        void Delete(T entity);
        void Delete(ICollection<T> entity);
        IQueryable<T> SearchFor(Expression<Func<T, bool>> predicate);
        IQueryable<T> GetAll();
        T GetById(int id);
    }

Generic repository;

    public class Repository<T> : IRepository<T> where T : class
    {
        protected DbSet<T> DbSet;

        public Repository(DbContext dataContext)
        {
            DbSet = dataContext.Set<T>();
        }

        public void Insert(T entity)
        {
            DbSet.Add(entity);
        }

        public void Insert(ICollection<T> entities)
        {
            DbSet.AddRange(entities);
        }

        public void Delete(T entity)
        {
            DbSet.Remove(entity);
        }

        public void Delete(ICollection<T> entities)
        {
            DbSet.RemoveRange(entities);
        }

        public IQueryable<T> SearchFor(Expression<Func<T, bool>> predicate)
        {
            return DbSet.Where(predicate);
        }

        public IQueryable<T> GetAll()
        {
            return DbSet;
        }

        public T GetById(int id)
        {
            return DbSet.Find(id);
        }
    }

Example use using a demo hotel class;

    var db = new DatabaseContext();
    var hotelRepo = new Repository<Hotel>(db);
    
    var hotel = new Hotel("Hotel 1", "42 Wallaby Way, Sydney");
    hotelRepo.Insert(hotel);
    db.SaveChanges();


