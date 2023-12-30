package org.chiu.megalith.blog.config;

import org.chiu.megalith.blog.http.OssHttpService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.support.WebClientAdapter;
import org.springframework.web.service.invoker.HttpServiceProxyFactory;

import java.time.Duration;

@Configuration(proxyBeanMethods = false)
public class HttpConfig {

    @Value("${blog.oss.bucket-name}")
    private String bucketName;

    @Value("${blog.oss.endpoint}")
    private String ep;

    @Value("${blog.oss.base-url}")
    private String baseUrl;

    @Bean
    OssHttpService ossHttpService() {

        WebClient client = WebClient.builder()
                .baseUrl(baseUrl)
                .defaultHeader(HttpHeaders.HOST, bucketName + "." + ep)
                .build();

        WebClientAdapter webClientAdapter = WebClientAdapter.create(client);
        webClientAdapter.setBlockTimeout(Duration.ofSeconds(10));

        HttpServiceProxyFactory factory = HttpServiceProxyFactory.builderFor(webClientAdapter)
                .build();
        return factory.createClient(OssHttpService.class);
    }
}