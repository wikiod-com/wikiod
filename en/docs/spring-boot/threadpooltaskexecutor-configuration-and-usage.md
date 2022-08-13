---
title: "ThreadPoolTaskExecutor configuration and usage"
slug: "threadpooltaskexecutor-configuration-and-usage"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

## application configuration
    @Configuration
    @EnableAsync
    public class ApplicationConfiguration{
        
        @Bean
        public TaskExecutor getAsyncExecutor() {
            ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
            executor.setCorePoolSize(2);
            executor.setThreadNamePrefix("executor-task-");
            executor.initialize();
            return executor;
        }
        
    }

