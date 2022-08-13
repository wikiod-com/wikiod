---
title: "Data Access Object(DAO) design pattern"
slug: "data-access-objectdao-design-pattern"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Data Access Object J2EE design pattern with Java
*Data Access Object(DAO)* design pattern is a standard J2EE design pattern. 

In this design pattern data is accessed through classes containing methods to access data from databases or other sources, which are called *data access objects*. Standard practice assumes that there are POJO classes. DAO can be mixed with other Design Patterns to access data, such as with MVC(model view controller), Command Patterns etc.

The following is an example of DAO design pattern. It has an **Employee** class, a DAO for Employee called **EmployeeDAO** and an **ApplicationView** class to demonstrate the examples.

**Employee.java**

    public class Employee {
        private Integer employeeId;
        private String firstName;
        private String lastName;
        private Integer salary;
        
        public Employee(){
            
        }
        
        public Employee(Integer employeeId, String firstName, String lastName, Integer salary) {
            super();
            this.employeeId = employeeId;
            this.firstName = firstName;
            this.lastName = lastName;
            this.setSalary(salary);
        }
    
        //standard setters and getters
    
    }

**EmployeeDAO**

    public class EmployeeDAO {
        
        private List<Employee> employeeList;
        
        public EmployeeDAO(List<Employee> employeeList){ 
            this.employeeList = employeeList;
        }
        
        public List<Employee> getAllEmployees(){
            return employeeList;
        }
        
        //add other retrieval methods as you wish
        public Employee getEmployeeWithMaxSalary(){
            Employee employee = employeeList.get(0);
            for (int i = 0; i < employeeList.size(); i++){
                Employee e = employeeList.get(i);
                if (e.getSalary() > employee.getSalary()){
                    employee = e;
                }
            }
            
            return employee;
        }
        
    }

**ApplicationView.java**

    public class ApplicationView {
    
        public static void main(String[] args) {
            // See all the employees with data access object
    
            List<Employee> employeeList = setEmployeeList();
            EmployeeDAO eDAO = new EmployeeDAO(employeeList);
    
            List<Employee> allEmployees = eDAO.getAllEmployees();
    
            for (int i = 0; i < allEmployees.size(); i++) {
                Employee e = employeeList.get(i);
                System.out.println("UserId: " + e.getEmployeeId());
            }
    
            Employee employeeWithMaxSalary = eDAO.getEmployeeWithMaxSalary();
    
            System.out.println("Maximum Salaried Employee" + " FirstName:" + employeeWithMaxSalary.getFirstName()
            + " LastName:" + employeeWithMaxSalary.getLastName() + " Salary: " + employeeWithMaxSalary.getSalary());
    
        }
    
        public static List<Employee> setEmployeeList() {
            Employee employee1 = new Employee(1, "Pete", "Samprus", 3000);
            Employee employee2 = new Employee(2, "Peter", "Russell", 4000);
            Employee employee3 = new Employee(3, "Shane", "Watson", 2000);
    
            List<Employee> employeeList = new ArrayList<>();
            employeeList.add(employee1);
            employeeList.add(employee2);
            employeeList.add(employee3);
            return employeeList;
        }
    
    }

Hence we have an example where we see how to use Data Access Object design pattern.



