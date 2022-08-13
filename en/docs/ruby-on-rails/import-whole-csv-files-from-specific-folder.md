---
title: "Import whole CSV files from specific folder"
slug: "import-whole-csv-files-from-specific-folder"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

In this example, lets say we have many product CSV files in a folder. Each CSV file need to upload our database from our console write a command. Run the following command in a new or existing project to create this model.


## Uploads CSV from console command
    
Terminal Commands:

    rails g model Product name:string quantity:integer price:decimal{12,2}
    rake db:migrate

Lates create controller.

Terminal Commands:

    rails g controller Products


Controller Code:

    class HistoriesController < ApplicationController
        def create
            file = Dir.glob("#{Rails.root}/public/products/**/*.csv") #=> This folder directory where read the CSV files
            file.each do |file|
                Product.import(file)
            end
        end
    end 

Model: 

    class Product< ApplicationRecord
      def self.import(file)
          CSV.foreach(file.path, headers: true) do |row|
              Product.create! row.to_hash
          end
      end
    end

routes.rb

    resources :products

app/config/application.rb

    require 'csv'


Now open your development `console` & `run`

    => ProductsController.new.create #=> Uploads your whole CSV files from your folder directory

