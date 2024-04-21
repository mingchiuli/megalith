package org.chiu.megalith.manage.req;

import org.chiu.megalith.manage.valid.ListValue;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import org.hibernate.validator.constraints.URL;

/**
 * @author mingchiuli
 * @create 2022-12-01 9:26 pm
 */
@Data
public class BlogEntityReq {

    private Long id;

    @NotBlank(message = "标题不能为空")
    private String title;

    @NotBlank(message = "摘要不能为空")
    private String description;

    @NotBlank(message = "内容不能为空")
    private String content;

    @ListValue(values = {0,1})
    private Integer status;

    @URL
    private String link;
}
