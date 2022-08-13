---
title: "Working with Databases"
slug: "working-with-databases"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Using Yii2 query builder
Yii2 provides efficient ways to retrieve data from the database.Consider an example of a simple employee table having fields ***emp_id, emp_name and emp_salary***. In order to retrieve the employee names and their salaries, we use the query. 

    select emp_name,emp_salary from employee

To generate the above query in Yii2, there are a lot of methods.One of the method is to use a ***yii\db\Quer**y* object.

    //creates a new \yii\db\Query() object
    $query=new \yii\db\Query(); 

    $rows=$query->select(['emp_name','emp_salary']) //specify required columns in an array
                 ->from('employee') //specify table name
                 ->all(); //returns an array of rows with each row being an associative array of name-value pairs.

We can make use of a foreach loop to loop through each name-value pair in the ***$rows*** array.

    foreach ($rows as $row) {
        echo "Employee Name: ".$row['emp_name'].",Employee Salary: ".$row['emp_salary']."<br>";
    }

This will output

   

>  Employee Name: Kiran,Employee Salary: 25000
> 
> 
>  Employee Name: Midhun,Employee Salary: 50000
> 
> Employee Name: Jishnu,Employee Salary: 20000
> 
> Employee Name: Ajith,Employee Salary: 25000
> 
>  Employee Name: Akshay,Employee Salary: 750000

**More Examples**

Suppose we need to find the name of employees whose salary is equal to 25000.We can write the query in sql as  

    select emp_name from employee where salary=25000

In Yii2, the code for generating the above query

    $query=new \yii\db\Query(); 

    $rows=$query->select(['emp_name']) 
                ->from('employee')
                ->where(['emp_salary'=>25000]) //specify the condition as an associative array where key is column name
                ->all(); 

If we need to find employee names whose salary is greater than 25000,We can write the code in Yii2 as

      $rows=$query->select(['emp_name']) 
            ->from('employee')
            ->where(['>','emp_salary', 25000]) 
    //Here first element in the above array specify relational operator used, second element specify the table name and third the value itself.
            ->all();  



    

 

## More condition checking using where()
Multiple conditions can be written using **where()** method as given below.

    // Creates a new \yii\db\Query() object
    $query = new \yii\db\Query();
    $rows = $query->select(['emp_name','emp_salary']) 
            ->from('employee')
            ->where(['emp_name' => 'Kiran', 'emp_salary' => 25000]) // Specify multiple conditions
            ->one(); // Returns the first row of the result
The above code will fetch an employee having the name **kiran** and salary **25000**. If multiple employees are satisfying the above condition, the call ***one()*** makes sure that only the first result is fetched. To fetch all results you should use ***all()***.

Note that if you use ***all()*** the result will always be an array; Even if there is only one or zero results. This array contains all results as arrays or is empty when no records match. The call ***one()*** will return the resulting array directly or false if the query doesn't return anything.

The equivalent code in sql is given below.



    select emp_name, emp_salary from employee where emp_name = 'Kiran' and emp_salary = 25000 limit 1;

An alternative way of writing the above query in Yii2 is given below.

    $rows = $query->select(['emp_name', 'emp_salary']) 
        ->from('employee')
        ->where(['emp_name' => 'Kiran'])
        ->andWhere(['emp_salary' => 25000])
        ->one();

Additional set of conditions can be specified using **andWhere**. This will be useful if we need to add additional condition checking to the query later.

Yet another way to specify multiple conditions is by making use of **operator format of** **where()** method.The above query can also be written as given below.

     $rows = $query->select(['emp_name','emp_salary']) 
        ->from('employee')
        ->where(['and', 'emp_name="kiran"', 'emp_salary=25000'])        
        ->one();

Here we specify the operator '**and**' as the first element in the array. Similarly we can also use '**or**', '**between**', '**not between**', '**in**', '**not in**', '**like**', '**or like**', '**not like**', '**or not like**', '**exists**', '**not exists**', '**>**', '**<=**' etc as operators.

**Examples of using 'in' and 'like'** 

Suppose we need to find the employees having salaries **20000, 25000 and 50000**. In normal sql we would write the query as 

    select * from employee where salary in (20000,25000,50000)

In Yii2 we can write this as given below.

    
    $rows = $query->from('employee')
            ->where(['emp_salary' => [20000,25000,50000]]) 
            ->all();

Another way of specifying the same condition is 

    $rows = $query->from('employee')
        ->where(['in', 'emp_salary', [20000,25000,50000]]) // Making use of operator format of where() method
        ->all();
Similarly '**not in**' can be specified instead of '**in**' if we want to get all employees not having salaries 20000, 25000 and 50000.

Now let us see some examples of using '**like**' inside where() condition. Suppose we need to find all employees having the string '**gopal**' in their name. The names can be venugopal, rajagopal, gopalakrishnan etc. The sql query is given below.

    select * from employee where emp_name like '%gopal%'

In Yii2 we will write this as 

     $rows = $query->from('employee')
            ->where(['like', 'emp_name', 'gopal']) // Making use of operator format of where() method
            ->all();

If we need to find all employees having the string '**gopal**' and '**nair**' in their name. We can write as 

       $rows = $query->from('employee')
            ->where(['like', 'emp_name', ['gopal','nair']]) // Making use of operator format of where() method
            ->all();
This would evaluate as

>  select * from employee where emp_name like '%gopal%' and '%nair%'

Similarly we can use '**not like**' to indicate all employees not having the string '**gopal**' and '**nair**' in their names.




    

## Using orderBy()
The orderBy() method specifies the ORDER BY fragment of a SQL query.For example consider our employee table having fields **emp_id, emp_first_name, emp_last_name and emp_salary**.Suppose we need to order the result by increasing order of employee salaries.We can do it in sql as given below.

    Select * from employee order by emp_salary

In yii2, we can build the query as given below

   

     //creates a new \yii\db\Query() object
        $query=new \yii\db\Query();

        $rows= $query->from('employee')->orderBy([
        'emp_salary' => SORT_ASC //specify sort order ASC for ascending DESC for descending      
        ])->all();


If we need to order the employees with their first name in ascending order and then their salaries in descending order, we can write it in plain sql as follows.

    Select * from employee order by emp_first_name ASC, emp_salary DESC

The equivalent sql can be build using yii2 as follows

    //creates a new \yii\db\Query() object
        $query=new \yii\db\Query();

        $rows= $query->from('employee')->orderBy([
        'emp_first_name' => SORT_ASC
        'emp_salary' => SORT_DESC      
        ])->all();

You can also specify ORDER BY using a string, just like you do when writing raw SQL statements. For example ,the above query can also be generated as given below.


    //creates a new \yii\db\Query() object
    $query=new \yii\db\Query();
    $rows=$query->from('employee')->orderBy('emp_first_name ASC, emp_salary DESC')->all();

You can call addOrderBy() to add additional columns to the ORDER BY fragment. For example

    //creates a new \yii\db\Query() object
    $query=new \yii\db\Query();
    $rows=$query->from('employee')->orderBy('emp_first_name ASC')
        ->addOrderBy('emp_salary DESC')->all();



