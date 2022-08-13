---
title: "Using Multiple Models In One View"
slug: "using-multiple-models-in-one-view"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

The main focus of this topic using multiple model class in view layer of MVC

## Using multiple model in a view with dynamic ExpandoObject
ExpandoObject (the `System.Dynamic` namespace) is a class that was added to the `.Net Framework 4.0`. This class allows us to dynamically add and remove properties onto an object at runtime. By using Expando object we can add our model classes into dynamically created Expando object. Following example explains how we can use this dynamic object.

Teacher and Student Model:

    public class Teacher  
    {  
        public int TeacherId { get; set; }    
        public string Name { get; set; }  
    } 
    
    public class Student  
    {  
        public int StudentId { get; set; }  
        public string Name { get; set; }  
    }

Teacher and Student List Methods:

    public List<Teacher> GetTeachers()  
    {  
        List<Teacher> teachers = new List<Teacher>();  
        teachers.Add(new Teacher { TeacherId = 1, Name = "Teacher1" });  
        teachers.Add(new Teacher { TeacherId = 2, Name = "Teacher2" });  
        teachers.Add(new Teacher { TeacherId = 3, Name = "Teacher3" });  
        return teachers;  
    }   
    
    public List<Student> GetStudents()  
    {  
        List<Student> students = new List<Student>();  
        students.Add(new Student { StudentId = 1, Name = "Student1"});  
        students.Add(new Student { StudentId = 2, Name = "Student2"});  
        students.Add(new Student { StudentId = 3, Name = "Student3"});  
        return students;  
    }
Controller (Using Dynamic Model):

    public class HomeController : Controller  
    {  
        public ActionResult Index()  
        {  
            ViewBag.Message = "Hello World";  
            dynamic mymodel = new ExpandoObject();  
            mymodel.Teachers = GetTeachers();  
            mymodel.Students = GetStudents();  
            return View(mymodel);  
        }  
    }
View:

    @using ProjectName ; // Project Name  
    @model dynamic  
    @{  
        ViewBag.Title = "Home Page";  
    }  
    <h2>@ViewBag.Message</h2>  
    
    <h2>Teacher List</h2>  
    
    <table>  
        <tr>  
            <th>Id</th>    
            <th>Name</th>  
        </tr>  
        @foreach (Teacher teacher in Model.Teachers)  
        {  
            <tr>  
                <td>@teacher.TeacherId</td>  
                <td>@teacher.Name</td>  
            </tr>  
        }  
    </table>  
    
    <h2>Student List</h2>  
    
    <table>  
        <tr>  
            <th>Id</th>  
            <th>Name</th>  
        </tr>  
        @foreach (Student student in Model.Students)  
        {  
            <tr>  
                <td>@student.StudentId</td>   
                <td>@student.Name</td>  
            </tr>  
        }  
    </table> 

 

