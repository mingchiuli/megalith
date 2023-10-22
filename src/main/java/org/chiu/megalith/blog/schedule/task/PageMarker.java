package org.chiu.megalith.blog.schedule.task;

import lombok.Data;

/**
 * @author mingchiuli
 * @create 2023-06-24 5:17 pm
 */
@Data
public class PageMarker {

    public volatile boolean fin;

    int curPageNo;
}
