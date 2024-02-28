package org.chiu.megalith.blog.config;

import org.chiu.megalith.blog.http.OssHttpService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.web.client.RestClient;
import org.springframework.web.client.support.RestClientAdapter;
import org.springframework.web.service.invoker.HttpServiceProxyFactory;


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

        RestClient client = RestClient.builder()
                .baseUrl(baseUrl)
                .defaultHeader(HttpHeaders.HOST, bucketName + "." + ep)
                .build();

        RestClientAdapter restClientAdapter = RestClientAdapter.create(client);

        HttpServiceProxyFactory factory = HttpServiceProxyFactory.builderFor(restClientAdapter)
                .build();
        return factory.createClient(OssHttpService.class);
    }
}
