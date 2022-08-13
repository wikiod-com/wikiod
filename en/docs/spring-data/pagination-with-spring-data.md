---
title: "Pagination with Spring Data"
slug: "pagination-with-spring-data"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Pagination by passing parmeter with custom query in spring data JPA

## Pagination by passing parmeter with custom query in spring data JPA
I use Spring Boot 1.4.4.RELEASE , with MySQL as the Database and Spring Data JPA abstraction to work with MySQL. Indeed ,it is the Spring Data JPA module that makes it so easy to set up Pagination in a Spring boot app in the first place.

Scenario
expose an endpoint /students/classroom/{id} . It will return a List of Students and other paging info(which we would see in a minute) based on the page and size parameters and classroomId that were passed along with it.

To begin with, i create a domain Student

    @Entity
    @Table(name = "student")
    public class Student {  
    
        @Id
        @GeneratedValue(strategy = GenerationType.IDENTITY)
        private Long id;
    
        @Column(name = "name")
        private String name;
    
        @NotNull
        @Column(name = "rollnumber", nullable = false)
        private Integer rollnumber;
    
        @Column(name = "date_of_birth")
        private LocalDate dateOfBirth;
    
        @Column(name = "address")
        private String address;
    
        @ManyToOne(optional = false)
        @NotNull
        private Classroom classroom;
    
    //getter and setter
    
    }


Student relate with classroom


    @Entity
    @Table(name = "classroom")
    public class Classroom  {  
    
        @Id
        @GeneratedValue(strategy = GenerationType.IDENTITY)
        private Long id;
    
        @Column(name = "standard")
        private String standard;
    
        @Column(name = "section")
        private String section;
    
        @Column(name = "year")
        private String year;
    
    //getter && setter
    
    }

we have RestController

    @RestController
    @RequestMapping("/api")
    public class StudentResource {
    
    private final StudentService studentService;
    
        public StudentResource(StudentService studentService) {
            this.studentService = studentService;
        }
    
     @GetMapping("/students/classroom/{id}")
        public ResponseEntity<Page<StudentDTO>> getAllStudentsBasedOnClassroom(@ApiParam Pageable pageable,@PathVariable Long id)
            throws URISyntaxException {           
            Page<StudentDTO> page = studentService.findByClassroomId(id, pageable);
            HttpHeaders headers = PaginationUtil.generatePaginationHttpHeaders(page, "/api/students/classroom");
            return new ResponseEntity<Page<StudentDTO>>(page, headers, HttpStatus.OK);
        }
    
    }

Notice that we haven’t passed RequestParams to our handler method . When the endpoint /students/classroom/1?page=0&size=3 is hit, Spring would automatically resolve the page and size parameters and create a Pageable instance . We would then pass this Pageable instance to the Service layer ,which would pass it to our Repository layer .

Service class 

    public interface StudentService {
    
      Page<StudentDTO> findByClassroomId(Long id,Pageable pageable);
    
    }


service impl (here i user StudentMapper to convert Class to DTOby using mapStruct or we can do manualy)

    @Service
    @Transactional
    public class StudentServiceImpl implements StudentService{
    
     private final StudentRepository studentRepository;
    
        private final StudentMapper studentMapper;
    
        public StudentServiceImpl(StudentRepository studentRepository, StudentMapper studentMapper) {
            this.studentRepository = studentRepository;
            this.studentMapper = studentMapper;
        }
    @Override
        public Page<StudentDTO> findByClassroomId(Long id, Pageable pageable) {
             log.debug("Request to get Students based on classroom : {}", id);
             Page<Student> result = studentRepository.findByClassroomId(id, pageable);
             return result.map(student -> studentMapper.studentToStudentDTO(student));
        }
    
    }

this is mapper interface

    @Mapper(componentModel = "spring", uses = {})
    public interface StudentMapper{
    
      StudentDTO   studentToStudentDTO(Student student);
    }

then in StudentRepository i worte custom method

    public interface StudentRepository extends JpaRepository<Student,Long> {
    
        Page<Student> findByClassroomId(Long id, Pageable pageable);    
        
    }

then it will give us all below information with respective data

    "last": false,
      "totalElements": 20,
      "totalPages": 7,
      "size": 3,
      "number": 0,
      "sort": null,
      "first": true,
      "numberOfElements": 3


