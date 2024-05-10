package org.chiu.megalith.search.req;

import org.chiu.megalith.infra.vaild.ListValue;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import org.hibernate.validator.constraints.URL;

/**
 * @author mingchiuli
 * @create 2022-12-23 8:10 pm
 */
@Data
public class WebsiteDocumentReq {

    private String id;

    @ListValue(values = {0, 1})
    private Integer status;

    @NotBlank
    private String title;

    @NotBlank
    private String description;

    @URL
    private String link;
}
