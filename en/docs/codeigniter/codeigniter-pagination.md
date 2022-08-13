---
title: "Codeigniter Pagination"
slug: "codeigniter-pagination"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## in this section i assume you know to calling helper, in the controller

    public function house()
    {
        $config['base_url']        = site_url().'/user/house/';
        $config['total_rows']    = $this->houses->select_row_house_design();
        $config['per_page']     = 12;
        $config['cur_tag_open'] = '<li><a><b>';
        $config['cur_tag_close'] = '</li></a></b>';
        $config['prev_tag_open'] = '<li>';
        $config['prev_tag_close'] = '</li>';
        $config['next_tag_open'] = '<li>';
        $config['next_tag_close'] = '</li>';
        $config['num_tag_open'] = '<li>';
        $config['num_tag_close'] = '</li>';
        $config['last_tag_open'] = '<li>';
        $config['last_tag_close'] = '</li>';
        $config['first_tag_open'] = '<li>';
        $config['first_tag_close'] = '</li>';
        $this->pagination->initialize($config);
        $from = $this->uri->segment('3');
        $data['design'] = $this->houses->select_all_house_design($config['per_page'],$from);
        $title['menu'] = 'house design';
        $this->load->view('user/template/header',$title);
        $this->load->view('user/house',$data);
        $this->load->view('user/template/footer');
    }

## in the view

  <div align="center">
    <nav aria-label="Page navigation">
      <ul class="pagination">
          <?php echo $this->pagination->create_links(); ?>
      </ul>
    </nav>
  </div>

