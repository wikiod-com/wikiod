---
title: "Constructor and OnInit"
slug: "constructor-and-oninit"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

In respect of ionic2 the `constructor`: in simple terms we use it to create instance of our plugins, services etc. for example: You have a page(view) where you want to show the list of all students, and you have a json file that contains all the students (this file is your data file) what you have to do is to create a service in this service you will create a method and hit a http.get request to get the json data, so here you need what? http simply do this way:



## Student Service Method example for using Http in constructor
    import {Http} from '@angular/http';
    @Injectable()
    export class StudentService{
        constructor(public http: Http){}
        getAllStudents(): Observable<Students[]>{
            return this.http.get('assets/students.json')
            .map(res => res.json().data)     
            }
        }
notice the constructor now again if we want to use this service method we will go to our view/page and :

    import {StudentService} from './student.service';
    import { SocialSharing } from '@ionic-native/social-sharing';
    export class HomePage implements OnInit {
    
      constructor(public _studentService: StudentService, public socialSharing: SocialSharing) {
       }

again notice the constructor here, we are creating an instance of StudentService in constructor and one more thing, we are using socialSharing plugin so to use that we are creating instance of that in constructor as well.

## ngOnInit method to get the list of students on view load
`OnInit`: this is really amazing thing in ionic2 or we can say in AngularJs2. With the same above example we can see what is ngOnInit is. So you are ready with the service method, now in your view/page you want that student list data available as soon as your view is going to appear, this should be the first operation happend automatically on load, because as the view load the student list should be visible. So the class implements OnInit and you define ngOnInit. Example:



## ngOnInit example to get the list of students on page/view
    export class HomePage implements OnInit {
    ...
    ....
    constructor(....){}
    
    ngOnInit(){
        this._studentService.getAllStudents().subscribe(
         (students: Students[]) => this.students = students, 
        )

