---
title: "Ajax custom pagination in cakephp 3.2"
slug: "ajax-custom-pagination-in-cakephp-32"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Ajax custom pagination in cakephp 3.2
Here i am going to explain the ajax custom  pagination in cakephp 3.2.
This one is easier to use and understandable.

Do this code inside any function and any controller you need. 

Make an ajax call for the pagination

    <script type="text/javascript">
 
    $(document).ready(function () { 
        
  
        $("#count-order-tr").load(strUrl + "http://test.com/Appadmins/ajaxLoadSalesListing"); //load initial records
       
 
        //executes code below when user click on pagination links
        $("#count-order-tr").on("click", ".pagination a", function (e) { 

            e.preventDefault(); 
    
            var page = $(this).attr("data-page");
    
               $("#count-order-tr").load(strUrl + "Appadmins/ajaxLoadSalesListing", {"page": page}, function (data) {
               
                
            });

        });
        

    });

</script>

Get the current requested page no (1,2,3...)on ajax post 

    public function ajaxLoadSalesListing() {
        $this->viewBuilder()->layout('ajax');
 

     if(isset($_POST["page"])){
 
        $page_number = filter_var($_POST["page"], FILTER_SANITIZE_NUMBER_INT, FILTER_FLAG_STRIP_HIGH); //filter number
 
        if(!is_numeric($page_number)){die('Invalid page number!');} //incase of invalid page number
 
    }else{ 
        $page_number = 1; //if there's no page number, set it to 1 
     }


    $item_per_page=5;//total number of records in a page.I have assumed it here 5
   
     $get_total_rows = $this->Models->find('all')->count(); //hold total records in variable
 
      $total_pages = ceil($get_total_rows/$item_per_page);//count total number of pages 
 
    $this->paginate['limit'=>$item_per_page ,'page'=>$page_number,'order'=>['Models.id'=>'DESC']];//Here we can give page ,limit ,order ,conditions and contain also.
 

    $lists=$this->paginate($this->Models);//this is important ,this will do all the stuffs.This one is used for the paginations.

 

Then set your data to ajax ctp.
 
      $this->set(compact('Lists','item_per_page','page_number','get_total_rows','total_pages'));}
 

Put pagination part in ajax ctp page only ,so that it will refresh every time and load all the datas.
 

Now fetch all the codes in the ctp page(ajax_load_sales_listing.ctp) and put this code inside it after the loop
 

  <?php     echo $this->Custom->paginate_function($item_per_page, $page_number, $get_total_rows, $total_pages);?>

Then call this paginate_function from custom helper.

    function paginate_function($item_per_page, $current_page, $total_records, $total_pages){
     $pagination = '';
    if($total_pages > 0 && $total_pages != 1 && $current_page <= $total_pages){ //verify total pages and current page number
        $pagination .= '<ul class="pagination">';
        
        $right_links    = $current_page + 3; 
        $previous       = $current_page - 3; //previous link 
        $next           = $current_page + 1; //next link
        $first_link     = true; //boolean var to decide our first link
        
        if($current_page > 1){
            $previous_link = ($previous==0)? 1: $previous;
            $pagination .= '<li class="first"><a href="javscript:;" data-page="1" title="First">&laquo;</a></li>'; //first link
            $pagination .= '<li><a href="javscript:;" data-page="'.($current_page-1).'" title="Previous">&lt;</a></li>'; //previous link
                for($i = ($current_page-2); $i < $current_page; $i++){ //Create left-hand side links
                    if($i > 0){
                        $pagination .= '<li><a href="#" data-page="'.$i.'" title="Page'.$i.'">'.$i.'</a></li>';
                    }
                }   
            $first_link = false; //set first link to false
        }
        
        if($first_link){ //if current active page is first link
            $pagination .= '<li class="first active">'.$current_page.'</li>';
        }elseif($current_page == $total_pages){ //if it's the last active link
            $pagination .= '<li class="last active">'.$current_page.'</li>';
        }else{ //regular current link
            $pagination .= '<li class="active">'.$current_page.'</li>';
        }
                
        for($i = $current_page+1; $i < $right_links ; $i++){ //create right-hand side links
            if($i<=$total_pages){
                $pagination .= '<li><a href="javscript:;" data-page="'.$i.'" title="Page '.$i.'">'.$i.'</a></li>';
            }
        }
        if($current_page < $total_pages){ 
                $next_link = ($i > $total_pages) ? $total_pages : $i;
                $pagination .= '<li><a href="javscript:;" data-page="'.($current_page+1).'" title="Next">&gt;</a></li>'; //next link
                $pagination .= '<li class="last"><a href="javscript:;" data-page="'.$total_pages.'" title="Last">&raquo;</a></li>'; //last link
        }
        
        $pagination .= '</ul>'; 
    }
    return $pagination; }}//return pagination links
 


Then pagination set .




 



