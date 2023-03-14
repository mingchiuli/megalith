package com.chiu.megalith.coop.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;

import java.io.Serializable;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class QuitDto extends MessageDto implements Serializable {}
