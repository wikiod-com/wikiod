---
title: "Pagination"
slug: "pagination"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

## Pagination Route Example with flask-sqlalchemy Paginate
In this example we use a parameter in the route to specify the page number. We set a default of 1 in the function parameter `page=1`. We have a `User` object in the database and we query it, ordering in descending order, showing latest users first. We then use the [`paginate`][1] method of the `query` object in flask-sqlalchemy. We then pass this to `render_template` to be rendered.

    @app.route('/users')
    @app.route('/users/page/<int:page>')
    def all_users(page=1):
        try:
            users_list = User.query.order_by(
                User.id.desc()
            ).paginate(page, per_page=USERS_PER_PAGE)
        except OperationalError:
            flash("No users in the database.")
            users_list = None

        return render_template(
            'users.html',
            users_list=users_list,
            form=form
        )


  [1]: http://flask-sqlalchemy.pocoo.org/2.1/api/#flask.ext.sqlalchemy.BaseQuery.paginate

## Rendering pagination in Jinja
Here we use the object that we passed to `render_template` to display the pages, the current active page, and also a previous and next buttons if you can go to the previous/next page.

    <!-- previous page -->
    {% if users_list.has_prev %}
    <li>
        <a href="{{ url_for('users', page=users_list.prev_num) }}">Previous</a>
    </li>
    {% endif %}
    
    <!-- all page numbers -->
    {% for page_num in users_list.iter_pages() %}
        {% if page_num %}
            {% if page_num != users_list.page %}
                <li>
                    <a href="{{ url_for('users', page=page_num) }}">{{ page_num }}</a>
                </li>
            {% else %}
                <li class="active">
                    <a href="#">{{ page_num }}</a>
                </li>
            {% endif %}
       {% else %}
           <li>
               <span class="ellipsis" style="white-space; nowrap; overflow: hidden; text-overflow: ellipsis">…</span>
           </li>
       {% endif %}
    {% endfor %}
    
    <!-- next page -->
    {% if users_list.has_next %}
    <li>
        <a href="{{ url_for('users', page=users_list.next_num) }}">Next</a></li>
    {% endif %}
    {% endif %}

