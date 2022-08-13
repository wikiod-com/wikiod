---
title: "unit testing"
slug: "unit-testing"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Basic unit test
component file
----------


    
    @Component({
      selector: 'example-test-compnent',
      template: '<div>
                      <div>{{user.name}}</div>
                      <div>{{user.fname}}</div>
                      <div>{{user.email}}</div>
                 </div>'
    })

    export class ExampleTestComponent implements OnInit{

        let user :User = null;
        ngOnInit(): void {
           this.user.name = 'name';
           this.user.fname= 'fname';
           this.user.email= 'email';
        }
        
    }


Test file


----------

    describe('Example unit test component', () => {
      let component: ExampleTestComponent ;
      let fixture: ComponentFixture<ExampleTestComponent >;
    
      beforeEach(async(() => {
        TestBed.configureTestingModule({
          declarations: [ExampleTestComponent]
        }).compileComponents();
      }));
    
      beforeEach(() => {
        fixture = TestBed.createComponent(ExampleTestComponent );
        component = fixture.componentInstance;
        fixture.detectChanges();
      });
      
    
      it('ngOnInit should change user object values', () => {
        expect(component.user).toBeNull(); // check that user is null on initialize
        component.ngOnInit(); // run ngOnInit
    
        expect(component.user.name).toEqual('name');
        expect(component.user.fname).toEqual('fname');
        expect(component.user.email).toEqual('email');
      });
    });

