package org.chiu.megalith.blog.event;

import lombok.Getter;
import lombok.Setter;
import org.chiu.megalith.infra.search.BlogSearchIndexMessage;
import org.springframework.context.ApplicationEvent;

@Getter
@Setter
public class BlogOperateEvent extends ApplicationEvent {

    private BlogSearchIndexMessage blogSearchIndexMessage;

    public BlogOperateEvent(Object source, BlogSearchIndexMessage blogSearchIndexMessage) {
        super(source);
        this.blogSearchIndexMessage = blogSearchIndexMessage;
    }
}
